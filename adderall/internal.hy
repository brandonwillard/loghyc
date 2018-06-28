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

;; (require hy.contrib.alias)
;; `hy.contrib.alias` was removed, so we simply add the provided functions.

(require [hy.contrib.walk [let]])


(defmacro defmacro-alias [names lambda-list &rest body]
  "define one macro with several names"
  (setv ret `(do))
  (for [name names]
    (.append ret
             `(defmacro ~name ~lambda-list ~@body)))
  ret)

(defmacro defn-alias [names lambda-list &rest body]
  "define one function with several names"
  (let [main (first names)
        aliases (list (rest names))]
       (setv ret `(do (defn ~main ~lambda-list ~@body)))
       (for [name aliases]
         (.append ret
                  `(setv ~name ~main)))
       ret))

(defn cons [a b]
  (+ (, a) (if (instance? (tuple [list tuple]) b)
               (tuple b)
               (, b))))

;; FIXME: We need a better/clear convention for what we consider a cons.
(defn cons? [a]
  (if (and (instance? tuple a)
           (> (len a) 0))
      True
   False))

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
  (while (lvar? val)
    (for [[svar sval] (substitutions s)]
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
          ;; FIXME: Now that there are no cons, how do we properly disinguish?
          [(cons? val) (cons (reifying (first val))
                             (reifying (list (rest val))))]
          [(seq? val) ((type val) (map reifying val))]
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
           ;; (any (list-comp (occurs var item s) [item val])))))

(defn neseq? [c]
  (and (seq? c) (pos? (len c))))

(defn setish-first [l]
  (if (instance? set l)
    (first (list l))
    (first l)))

(defn setish-rest [l]
  (if (instance? set l)
      ((type l) (list (rest (list l))))
   (list (rest l))))

(defn unify [u v s]
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
     (setv s (unify (first u) (setish-first v) s))
     (setv s (unify (list (rest u)) (setish-rest v) s))
     s)]
   [(= u v) s]))

(defn __all__ [])
