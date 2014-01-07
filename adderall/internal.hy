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

(import [adderall.lvar [LVar]]
        [hy [HyCons]])

(defn gen-solutions [var goal]
  (list-comp (reify var opt-s)
             [opt-s (goal (tuple []))]
             (not (nil? opt-s))))

(defn gen-goals [f goals]
  (if (= (len goals) 1)
   (get goals 0)
   (apply f goals)))

(defn run! [f var goals]
  (gen-solutions var (gen-goals f goals)))

(defn lvar? [x] (instance? LVar x))
(defn tuple? [x] (instance? tuple x))
(defn seq? [x] (or (tuple? x)
                   (instance? list x)))
(defn cons? [x] (instance? HyCons x))

(defn substitute [val s]
  (while (lvar? val)
    (for* [[svar sval] (substitutions s)]
      (when (is val svar)
        (setv val sval)
        (break))
      (else (break))))
  val)

(defn substitutions [s]
  (while (not (is s (tuple [])))
    (setv (, var val s) s)
    (yield (tuple [var val]))))

(defn free-lvar [id]
  (LVar (.format "_.{0}" id)))

(defn reify [val s]
  (setv free-vars {})
  (defn reifying [val]
    (setv val (substitute val s))
    (cond [(lvar? val) (do
                        (unless (in val free-vars)
                          (setv (get free_vars val)
                                (free-lvar (len free-vars))))
                        (get free-vars val))]
          [(seq? val) ((type val) (map reifying val))]
          [(cons? val) (cons (reifying (first val))
                             (reifying (rest val)))]
          [true val]))
  (reifying val))

(defn unbound? [l]
  (and (lvar? l)
       (.startswith l.name "_.")))

(defn cons-substitute [c s]
  (cond
   [(cons? c) (cons (if (unbound? (first c))
                      (reify (first c) s)
                      (first c)) (cons-substitute (rest c) s))]
   [(lvar? c) (reify c s)]
   [True c]))

(defn extend-unchecked [var val s]
  (if (cons? val)
    (tuple [var (cons-substitute val s) s])
    (tuple [var val s])))

(defn extend [var val s]
  (unless (occurs var val s)
    (extend-unchecked var val s)))

(defn occurs [var val s]
  (setv val (substitute val s))
  (or (is var val)
      (and (tuple? val)
           (any (list-comp (occurs var item s) [item val])))))

(defn neseq? [c]
  (and (seq? c) (> (len c) 0)))

(defn unify [u v s]
  (setv u (substitute u s))
  (setv v (substitute v s))

  (cond
   [(is u v) s]
   [(lvar? u) (if (lvar? v) (extend-unchecked u v s) (extend u v s))]
   [(lvar? v) (extend v u s)]
   [(and (seq? u) (seq? v) (= (len u) (len v)))
    (do
     (for [[ui vi] (zip u v)]
       (setv s (unify ui vi s))
       (if (nil? s)
         (break)))
     s)]
   [(or (and (cons? u) (or (cons? v) (neseq? v)))
        (and (or (cons? v) (neseq? u)) (cons? v)))
    (do
     (setv s (unify (first u) (first v) s))
     (setv s (unify (rest u) (rest v) s))
     s)]
   [(= u v) s]))

(defn interleave [iters]
  (while iters
    (try
     (yield (next (get iters 0)))
     (catch [e StopIteration] (setv iters (slice iters 1)))
     (else (setv iters (+ (slice iters 1) [(get iters 0)]))))))
