;; adderall - miniKanren in Hy
;; Copyright (C) 2014  Gergely Nagy <algernon@madhouse-project.org>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(import [itertools [islice]]
        [functools [reduce]]
        [adderall.internal [unify lvar? seq? reify]]
        [adderall.lvar [LVar unbound]])
(require adderall.internal)

;; Top level stuff

(defmacro run [n vars &rest goals]
  (let [[s (gensym)]]
    `(do
      (let [~@(list-comp `[~x (LVar (gensym))] [x vars])
            [res (fn [] (for [~s ((apply allᵍ [~@goals]) (,))]
                         (when (nil? ~s)
                           (continue))
                         (yield (reify (if (= (len ~vars) 1)
                                         (first ~vars)
                                         [~@vars]) ~s))))]]
        (if ~n
          (list (islice (res) 0 ~n))
          (list (res)))))))
(defmacro run* [var &rest goals]
  `(run nil ~var ~@goals))

(defmacro fresh [vars &rest goals]
  (cond
   [goals `(let [~@(list-comp `[~x (LVar (gensym '~x))] [x vars])]
             (allᵍ ~@goals))]
   [(= (len vars) 1) `(LVar (gensym '~(first vars)))]
   [true `[~@(list-comp `(LVar (gensym '~x)) [x vars])]]))

;; Goals

(defn-alias [≡ == unifyo] [u v]
  (fn [s]
    (yield (unify u v s))))

(defn succeed [s]
  (yield s))
(defreader s [_] succeed)

(defn fail [s]
  (iter ()))
(defreader u [_] fail)

(defn-alias [bothᵍ bothg] [g1 g2]
  (fn [s]
    (for [opt-s1 (g1 s)]
      (unless (nil? opt-s1)
        (for [opt-s2 (g2 opt-s1)]
          (yield opt-s2))))))

(defn-alias [allᵍ allg] [&rest goals]
  (reduce bothg goals))

(defmacro-alias [eitherᵍ eitherg] [&rest goals]
  `(fn [s]
     (for [goal [~@goals]]
       (for [r (goal s)]
         (yield r)))))

(defmacro Zzz [g]
  `(fn [s] (~g s)))

(defmacro-alias [condᵉ conde] [&rest cs]
  (let [[g (first cs)]
        [r (rest cs)]]
    (if r
      `(eitherᵍ (Zzz (allᵍ ~@g)) (Zzz (condᵉ ~@r)))
      `(Zzz (allᵍ ~@g)))))

(defn-alias [consᵒ conso] [f r l]
  (cond
   [(or (is r nil) (= r [])) (≡ [f] l)]
   [(or (lvar? r) (seq? r)) (≡ (cons f r) l)]
   [true (≡ (cons f [r]) l)]))

(defn-alias [firstᵒ firsto] [l a]
  (fresh [d]
         (consᵒ a d l)))

(defn-alias [restᵒ resto] [l d]
  (fresh [a]
         (≡ (cons a d) l)))

(defn-alias [emptyᵒ emptyo] [l]
  (≡ [] l))

(defn-alias [eqᵒ eqo] [u v]
  (≡ u v))

(defn-alias [pairᵒ pairo] [l]
  (fresh [a d]
         (consᵒ a d l)))

(defn-alias [listᵒ listo] [l]
  (condᵉ
   [(emptyᵒ l) succeed]
   [(pairᵒ l) (fresh [d]
                     (restᵒ l d)
                     (listᵒ d))]))

(defn-alias [lolᵒ lolo] [l]
  (condᵉ
   [(emptyᵒ l) #ss]
   [(fresh [a]
           (firstᵒ l a)
           (listᵒ a))
    (fresh [d]
           (restᵒ l d)
           (lolᵒ d))]))

(defn-alias [twinsᵒ twinso] [s]
  (fresh [x]
         (≡ [x x] s)))

(defn-alias [lotᵒ loto] [l]
  (listofᵒ twinsᵒ l))

(defn-alias [listofᵒ listofo] [predᵒ l]
  (condᵉ
   [(emptyᵒ l) #ss]
   [(fresh [a]
           (firstᵒ l a)
           (predᵒ a))
    (fresh [d]
           (restᵒ l d)
           (listofᵒ predᵒ d))]))

(defn-alias [memberᵒ membero] [x l]
  (condᵉ
   [(firstᵒ l x) #ss]
   [#ss (fresh [d]
               (restᵒ l d)
               (memberᵒ x d))]))

(defn-alias [pmemberᵒ pmembero] [x l]
  (condᵉ
   [(firstᵒ l x)
    (fresh [a d]
           (restᵒ l (cons a d)))]
   [(firstᵒ l x) (restᵒ l [])]
   [#ss (fresh [d]
               (restᵒ l d)
               (pmemberᵒ x d))]))

(defn-alias [memberrevᵒ memberrevo] [x l]
  (condᵉ
   [#ss (fresh [d]
               (restᵒ l d)
               (memberrevᵒ x d))]
   [#ss (firstᵒ l x)]))
