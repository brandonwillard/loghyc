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
        [adderall.internal [unify interleave lvar? seq? reify]]
        [adderall.lvar [LVar unbound]])

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

(defn ≡ [u v]
  (fn [s]
    (yield (unify u v s))))
(def == ≡)
(def unifyo ≡)

(defn succeed [s]
  (yield s))
(defreader s [_] succeed)

(defn fail [s]
  (iter ()))
(defreader u [_] fail)

(defn bothᵍ [g1 g2]
  (fn [s]
    (for [opt-s1 (g1 s)]
      (unless (nil? opt-s1)
        (for [opt-s2 (g2 opt-s1)]
          (yield opt-s2))))))
(def bothg bothᵍ)

(defn allᵍ [&rest goals]
  (reduce bothg goals))
(def allg allᵍ)

(defn eitherᵍ [&rest goals]
  (fn [s]
    (interleave (list-comp (goal s) [goal goals]))))
(def eitherg eitherᵍ)

(defmacro condᵉ [&rest cs]
  (let [[g (first cs)]
        [r (rest cs)]]
    (if r
      `(eitherᵍ (allᵍ ~@g) (condᵉ ~@r))
      `(allᵍ ~@g))))

(defmacro conde [&rest cs]
  `(condᵉ ~@cs))

(defn consᵒ [f r l]
  (cond
   [(or (is r nil) (= r [])) (≡ [f] l)]
   [(or (lvar? r) (seq? r)) (≡ (cons f r) l)]
   [true (≡ (cons f [r]) l)]))
(def conso consᵒ)

(defn firstᵒ [l a]
  (fresh [d]
         (consᵒ a d l)))
(def firsto firstᵒ)

(defn restᵒ [l d]
  (fresh [a]
         (≡ (cons a d) l)))
(def resto restᵒ)

(defn emptyᵒ [l]
  (≡ [] l))
(def emptyo emptyᵒ)

(defn eqᵒ [u v]
  (≡ u v))
(def eqo eqᵒ)

(defn pairᵒ [l]
  (fresh [a d]
         (consᵒ a d l)))
(def pairo pairᵒ)

(defn listᵒ [l]
  (condᵉ
   [(emptyᵒ l) succeed]
   [(pairᵒ l) (fresh [d]
                     (restᵒ l d)
                     (listᵒ d))]))
(def listo listᵒ)

(defn lolᵒ [l]
  (condᵉ
   [(emptyᵒ l) #ss]
   [(fresh [a]
           (firstᵒ l a)
           (listᵒ a))
    (fresh [d]
           (restᵒ l d)
           (lolᵒ d))]))
(def lolo lolᵒ)

(defn twinsᵒ [s]
  (fresh [x]
         (≡ [x x] s)))
(def twinso twinsᵒ)

(defn lotᵒ [l]
  (listofᵒ twinsᵒ l))
(def loto lotᵒ)

(defn listofᵒ [predᵒ l]
  (condᵉ
   [(emptyᵒ l) #ss]
   [(fresh [a]
           (firstᵒ l a)
           (predᵒ a))
    (fresh [d]
           (restᵒ l d)
           (listofᵒ predᵒ d))]))
(def listofo listofᵒ)

(defn memberᵒ [x l]
  (condᵉ
   [(firstᵒ l x) #ss]
   [#ss (fresh [d]
               (restᵒ l d)
               (memberᵒ x d))]))
(def membero memberᵒ)

(defn pmemberᵒ [x l]
  (condᵉ
   [(firstᵒ l x)
    (fresh [a d]
           (restᵒ l (cons a d)))]
   [(firstᵒ l x) (restᵒ l [])]
   [#ss (fresh [d]
               (restᵒ l d)
               (pmemberᵒ x d))]))
(def pmembero pmemberᵒ)

(defn memberrevᵒ [x l]
  (condᵉ
   [#ss (fresh [d]
               (restᵒ l d)
               (memberrevᵒ x d))]
   [#ss (firstᵒ l x)]))
(def memberrevo memberrevᵒ)
