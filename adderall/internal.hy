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

(import [collections.abc [Iterable]])

(require [hy.contrib.walk [let]])


(defmacro defmacro-alias [names lambda-list &rest body]
  "define one macro with several names"
  (+ `(do) (lfor name names `(defmacro ~name ~lambda-list ~@body))))

(defmacro defn-alias [names lambda-list &rest body]
  "define one function with several names"
  (let [main (first names)
        aliases (list (rest names))]
    (+ `(do (defn ~main ~lambda-list ~@body))
       (lfor name aliases `(setv ~name ~main)))))

(defclass Nil/cl [] 
  (defn --init-- [self]
    (setv
      self.car self
      self.cdr self)))
(setv nil/cl (Nil/cl))
(defn null/cl? [x]
  (cond
    [(instance? Nil/cl x) True]
    [(= [] x) True]
    [(= (,) x) True]
    [(= '() x) True]
    [True False])) ;;None is not nil

(defclass ConsPair [Iterable]
  "Construct a cons list.

A Python `list` is returned when the cdr is a `list` or `None`; otherwise, a
`ConsPair` is returned.

The arguments to `ConsPair` can be a car & cdr pair, or a sequence of objects to
be nested in `cons`es, e.g.

    (ConsPair car-1 car-2 car-3 cdr) == (ConsPair car-1 (cons car-2 (cons car-3 cdr)))
"
  (defn --new-- [cls &rest parts]
    (if (> (len parts) 2)
        (reduce (fn [x y] (ConsPair y x))
                (reversed parts))
        ;; Handle basic car, cdr case.
        (do (setv car-part (-none-to-empty-or-list
                             (first parts)))
            (setv cdr-part (-none-to-empty-or-list
                             (if
                               (and (coll? parts)(> (len parts) 1))
                               (last parts)
                               ;;None
                               nil/cl
                               )))
            (cond
              [(instance? Nil/cl cdr-part)
               `(~car-part)]
              [(instance? hy.models.HyExpression cdr-part)
               `(~car-part ~@cdr-part)]
              [(tuple? cdr-part)
               (tuple (+ [car-part] (list cdr-part)))]
              [(list? cdr-part)
               ;; Try to preserve the exact type of list
               ;; (e.g. in case it's actually a HyList).
               (+ ((type cdr-part) [car-part]) cdr-part)]
              [True
               (do
                 (setv instance (.--new-- (super ConsPair cls) cls))
                 (setv instance.car car-part)
                 (setv instance.cdr cdr-part)
                 instance)]))))
  (defn --hash-- [self]
    (hash [self.car, self.cdr]))
  (defn --eq-- [self other]
    (and (= (type self) (type other))
         (= self.car other.car)
         (= self.cdr other.cdr)))
  (defn --iter-- [self]
    (yield self.car)
    (if (coll? self.cdr) ;;(list? self.cdr)
        (for [x self.cdr] (yield x))
        (raise StopIteration))
    )
  (defn --repr-- [self]
    (.format "({} . {})" self.car self.cdr)))

(defn -none-to-empty-or-list [x]
  (cond
    ;;[(none? x) (list)]
    [(tuple? x) x]
    [(and (coll? x)
          (not (cons? x))
          (not (list? x)))
     (list x)]
    [True x]))

;; A synonym for ConsPair
(setv cons ConsPair)

(defn car [z]
  (or (getattr z "car" None)
      (-none-to-empty-or-list (first z))))
(defn cdr [z]
  (or (getattr z "cdr" None)
      ;; Try to preserve the exact type of list
      ;; (e.g. in case it's actually a HyList).
      ((type z) (list (rest z)))))

(defn lvar? [x] (instance? LVar x))
(defn seq? [x] (or (and (not (cons? x))
                        (tuple? x))
                   (instance? list x)
                   (instance? set x)))
(defn neseq? [c]
  (and (seq? c) (not (empty? c))))
(defn cons? [a]
  (cond [(null/cl? a) False]
        [(instance? ConsPair a) True]
        ;;[(coll? a) (not (empty? a)) True]
        [(or (list? a) (tuple? a) ) (not (empty? a)) True]
        [True False]))


(defn interleave [seqs]
  (setv iters (map iter seqs))
  (while iters
    (setv newiters [])
    (for [itr iters]
      (try
        (do
          (yield (next itr))
          (.append newiters itr))
        (except [StopIteration])))
    (setv iters newiters)))

(defclass LVar [object]
  (defn --init-- [self name &optional unbound]
    (setv self.name name)
    (when unbound
      (setv self.unbound True)))
  (defn --hash-- [self]
    (hash self.name))
  (defn --eq-- [self other]
    (and (= (type self) (type other))
         (= self.name other.name)))
  (defn --repr-- [self]
    (.format "<{0!r}>" self.name))
  (defn bound? [self]
    (if self.unbound
        True
        False)))

(defn unbound [n]
  (LVar (.format "_.{0}" n) 'unbound))

(defn substitute [val s]
  "Substitute mapped values found in the given alist.

`s` must be an alist represented by a tuple."
  (while (lvar? val)
    (for [[svar sval] (substitutions s)]
      (when (is val svar)
        (setv val sval)
        (break))
      (else (break))))
  val)

(defn substitutions [s]
  "Generator for alist (in the form of a tuple) key-value pairs."
  (while (!= s (,))
    (setv (, var val s) s)
    (yield (, var val))))

(defn reify [val s]
  "Replace LVars with their unified values.

E.g.
  (setv unify-form ['a (LVar 'b)])
  (setv unify-res (unify ['a 'b] unify-form (tuple)))
  (reify unify-form unify-res)

Returns `val` with LVars substituted by their associated values in the
unification results, `s`.
"
  (setv free-vars {})
  (defn reifying [val]
    (setv val (substitute val s))
    (cond [(lvar? val) (do
                         (unless (in val free-vars)
                           (setv (get free_vars val)
                                 (unbound (len free-vars))))
                         (get free-vars val))]
          [(seq? val) ((type val) (map reifying val))]
          [(cons? val) (cons (reifying (car val))
                             (reifying (cdr val)))]
          [True val]))
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
           (any (lfor item val (occurs var item s))))))

(defn unify [u v s]
  "Unify two forms, given a tuple of existing substitutions.

E.g.
  (unify ['a 'b 'c] ['a (LVar 'b) (LVar 'c)] (tuple))
"
  (when s
    (setv u (substitute u s))
    (setv v (substitute v s)))
  (cond
    [(none? s) s]
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
         (when (none? s)
           (break)))
       s)]
    [(or (and (cons? u) (or (cons? v) (neseq? v)))
         (and (or (cons? u) (neseq? u)) (cons? v)))
     (do
       (setv s (unify (car u) (car v) s))
       (setv s (unify (cdr u) (cdr v) s))
       s)]
    [(= u v) s]))

(setv EXPORTS [])
