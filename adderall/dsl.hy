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
        [adderall.internal [*]]
        [adderall.lvar [LVar]]
        [hy.models.list [HyList]])

;; Top level stuff

(defn run* [var &rest goals]
  (list (run! allᵍ var goals)))
(def run_all run*)

(defn run [n var &rest goals]
  (list (islice (run! allᵍ var goals) 0 n)))

(defmacro fresh [vars &rest goals]
  (cond
   [goals `(let [~@(list-comp `[~x (LVar (gensym '~x))] [x vars])]
             (allᵍ ~@goals))]
   [(= (len vars) 1) `(LVar (gensym '~(first vars)))]
   [true `[~@(list-comp `(LVar (gensym '~x)) [x vars])]]))

;; Goals

(defn ≣ [u v]
  (fn [s]
    (yield (unify u v s))))
(def == ≣)

(defn succeed [s]
  (yield s))
(defreader s [_] succeed)

(defn fail [s]
  (iter ()))
(defreader u [_] fail)

(defn bothᵍ [g1 g2]
  (fn [s]
    (for [opt-s1 (g1 s)]
      (unless (is opt-s1 nil)
        (for [opt-s2 (g2 opt-s1)]
          (yield opt-s2))))))
(def bothg bothᵍ)

(defn allᵍ [goal &rest goals]
  (if goals
    (bothᵍ goal (apply allᵍ goals))
    goal))
(def allg allᵍ)

(defn eitherᵍ [&rest goals]
  (fn [s]
    (interleave (list-comp (goal s) [goal goals]))))
(def eitherg eitherᵍ)

(defn condᵉ [c &rest cs]
  (let [[goal (apply allᵍ c)]]
    (if cs
      (eitherᵍ goal (apply condᵉ cs))
      goal)))
(def conde condᵉ)

(defn consᵒ [f r l]
  (cond
   [(or (is r nil) (= r [])) (≣ [f] l)]
   [(or (lvar? r) (seq? r)) (≣ (cons f r) l)]
   [true (≣ (cons f [r]) l)]))
(def conso consᵒ)

(defn firstᵒ [l a]
  (fresh [d]
         (consᵒ a d l)))
(def firsto firstᵒ)

(defn restᵒ [l d]
  (fresh [a]
         (≣ (cons a d) l)))
(def resto restᵒ)

(defn nilᵒ [l]
  (≣ [] l))
(def nilo nilᵒ)

(defn eqᵒ [u v]
  (≣ u v))

(defn pairᵒ [l]
  (fresh [a d]
         (consᵒ a d l)))
(def pairo pairᵒ)
