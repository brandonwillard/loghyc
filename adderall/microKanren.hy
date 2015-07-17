;; adderall - miniKanren in Hy
;; Copyright (C) 2014, 2015  Gergely Nagy <algernon@madhouse-project.org>
;;
;; This library is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program. If not, see <http://www.gnu.org/licenses/>.

(defn var [c]
  [c])

(defn var? [x]
  (coll? x))

(defn var=? [x1 x2]
  (= (car x1) (car x2)))

(defn walk [u s]
  (let [[pr (and (var? u)
                 (list (filter (lambda [v]
                                 (var=? u v))
                               s)))]]
    (if pr
      (walk (cdr pr) s)
      u)))

(defn ext-s [x v s]
  (cons (cons x v) s))

(defn == [u v]
  (fn [sc]
    (let [[s (unify u v (car sc))]]
      (if s
        (unit (cons s (cdr sc)))
        mzero))))

(defn unit [sc]
  (cons sc mzero))

(def mzero '())

(defn unify [u v s]
  (let [[uw (walk u s)]
        [vw (walk v s)]]
    (cond
     [(and (var? uw) (var? vw) (var=? uw vw)) s]
     [(var? uw) (ext-s uw vw s)]
     [(var? vw) (ext-s vw uw s)]
     [(and (cons? uw) (cons? vw))
      (let [[s (unify (car uw) (car vw) s)]]
        (and s (unify (cdr uw) (cdr vw) s)))]
     [true (and (= uw vw) s)])))

(defn call/fresh [f]
  (fn [sc]
    (let [[c (cdr sc)]]
      ((f (var c))
       (cons (car sc)
             (+ c 1))))))

(defn disj [g1 g2]
  (fn [sc]
    (mplus (g1 sc) (g2 sc))))

(defn conj [g1 g2]
  (fn [sc]
    (bind (g1 sc) g2)))

(defn mplus [v1 v2]
  (print "D" v1 v2)
  (cond
   [(nil? v1) v2]
   [(callable v1) (fn [] (mplus v2 (v1)))]
   [true (cons (first v1)
               (mplus (cdr v1) v2))]))

(defn bind [v g]
  (cond
   [(nil? v) mzero]
   [(callable v) (fn [] (bind (v) g))]
   [true (mplus (g (first v))
                (bind (cdr v) g))]))
