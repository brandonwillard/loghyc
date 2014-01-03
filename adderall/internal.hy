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

(import [adderall.lvar [LVar]])

(defn gen-solutions [var goal]
  (list-comp (reify var opt-s)
             [opt-s (goal (tuple []))]
             (not (is opt-s nil))))

(defn lvar? [x] (isinstance x LVar))
(defn tuple? [x] (isinstance x tuple))

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

(defn reify [val s]
  (setv free_vars {})
  (let [[reifying
         (fn [val]
           (setv val (substitute val s))
           (cond [(lvar? val) (do
                               (if (not (in val free_vars))
                                 (setv (get free_vars val)
                                       (LVar (.format "_.{0}"
                                                      (len free_vars)))))
                               (get free_vars val))]
                 [(tuple? val) (tuple (map reifying val))]
                 [True val]))]]
    (reifying val)))

(defn extend-unchecked [var val s]
  (tuple [var val s]))

(defn extend [var val s]
  (when (not (occurs var val s))
    (extend-unchecked var val s)))

(defn occurs [var val s]
  (setv val (substitute val s))
  (or (is var val)
      (and (tuple? val)
           (any (list-comp (occurs var item s) [item val])))))

(defn unify [u v s]
  (setv u (substitute u s))
  (setv v (substitute v s))

  (cond
   [(is u v) s]
   [(lvar? u) (if (lvar? v) (extend-unchecked u v s) (extend u v s))]
   [(lvar? v) (extend v u s)]
   [(and (tuple? u) (tuple? v) (= (len u) (len v)))
    (do
     (for [[ui vi] (zip u v)]
       (setv s (unify ui vi s))
       (if (is s nil)
         (break)))
     s)]
   [(= u v) s]))
