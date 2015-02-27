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

(defn lvar? [x] (instance? LVar x))
(defn tuple? [x] (instance? tuple x))
(defn seq? [x] (or (tuple? x)
                   (instance? list x)
                   (instance? set x)))

(defn interleave [seqs]
  (setv iters (map iter seqs))
  (while iters
    (setv newiters [])
    (for [itr iters]
      (try
       (do
        (yield (next itr))
        (.append newiters itr))
       (catch [StopIteration])))
    (setv iters newiters)))

(defclass LVar [object]
  [[--init-- (fn [self name &optional unbound]
               (setv self.name name)
               (when unbound
                 (setv self.unbound true))
               nil)]
   [--hash-- (fn [self]
               (hash self.name))]
   [--eq--   (fn [self other]
               (and (= (type self) (type other))
                    (= self.name other.name)))]
   [--repr-- (fn [self]
               (.format "<{0!r}>" self.name))]
   [bound? (fn [self]
             (if self.unbound
               true
               false))]])

(defn unbound [n]
  (LVar (.format "_.{0}" n) :unbound))

(defn substitute [val s]
  (while (lvar? val)
    (for* [[svar sval] (substitutions s)]
      (when (is val svar)
        (setv val sval)
        (break))
      (else (break))))
  val)

(defn substitutions [s]
  (while (!= s (,))
    (setv (, var val s) s)
    (yield (, var val))))

(defn reify [val s]
  (setv free-vars {})
  (defn reifying [val]
    (setv val (substitute val s))
    (cond [(lvar? val) (do
                        (unless (in val free-vars)
                          (setv (get free_vars val)
                                (unbound (len free-vars))))
                        (get free-vars val))]
          [(seq? val) ((type val) (map reifying val))]
          [(cons? val) (cons (reifying (first val))
                             (reifying (cdr val)))]
          [true val]))
  (reifying val))

(defn extend-unchecked [var val s]
  (, var val s))

(defn extend [var val s]
  (unless (occurs var val s)
    (extend-unchecked var val s)))

(defn occurs [var val s]
  (setv val (substitute val s))
  (or (is var val)
      (and (tuple? val)
           (any (list-comp (occurs var item s) [item val])))))

(defn neseq? [c]
  (and (seq? c) (pos? (len c))))

(defn setish-first [l]
  (if (instance? set l)
    (first (list l))
    (first l)))

(defn setish-rest [l]
  (if (instance? set l)
    ((type l) (cdr (list l)))
    (cdr l)))

(defn unify [u v s]
  (when s
    (setv u (substitute u s))
    (setv v (substitute v s)))

  (cond
   [(nil? s) s]
   [(is u v) s]
   [(and (hasattr u "unify")
         (callable u.unify))
    (.unify u v u s)]
   [(and (hasattr v "unify")
         (callable v.unify))
    (.unify v u v s)]
   [(lvar? u)
    (if (lvar? v)
      (extend-unchecked u v s)
      (if (and (hasattr v "unify")
               (callable v.unify))
        (.unify v u v s)
        (extend u v s)))]
   [(lvar? v)
    (if (and (hasattr u "unify")
             (callable u.unify))
      (.unify u v u s)
      (extend v u s))]
   [(and (seq? u) (seq? v) (= (len u) (len v)))
    (do
     (for [[ui vi] (zip u v)]
       (setv s (unify ui vi s))
       (when (nil? s)
         (break)))
     s)]
   [(or (and (cons? u) (or (cons? v) (neseq? v)))
        (and (or (cons? u) (neseq? u)) (cons? v)))
    (do
     (setv s (unify (first u) (setish-first v) s))
     (setv s (unify (cdr u) (setish-rest v) s))
     s)]
   [(= u v) s]))

(def __all__ [])
