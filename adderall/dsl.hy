;; adderall - miniKanren in Hy
;; Copyright (C) 2014, 2015, 2016  Gergely Nagy <algernon@madhouse-project.org>
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

(import [functools [reduce partial]]
        [hy [HySymbol HyExpression HyList HyKeyword]]
        [hy.contrib.walk [prewalk]])
(import [adderall.internal [unify lvar? seq? reify
                            LVar unbound interleave cons]])

(require [hy.contrib.walk [let]])
(require [monaxhyd.core [defmonad monad with-monad]])
(require [adderall.internal [defn-alias defmacro-alias]])

;; Top level stuff

(eval-and-compile
 (defn --prep-fresh-vars-- [vars]
   (setv fresh-vars [])
   (for [x vars]
     (.extend fresh-vars `[~x (LVar (gensym '~x))]))
   fresh-vars)

 (defn --prep-lvars-from-expr-- [lvars expr]
   (when (and (instance? HySymbol expr)
              (.startswith expr "?"))
     (.add lvars expr))
   expr))

(defmacro/g! lazy-run [n vars &rest goals]
  `(do
     (require [hy.contrib.walk [let :as ~g!let]])
     (~g!let [~@(--prep-fresh-vars-- vars)
              ~g!res (fn []
                       (for [~g!s ((all ~@goals) (,))]
                         (when (none? ~g!s)
                           (continue))
                         (yield (reify (if (= (len ~vars) 1)
                                           (first ~vars)
                                           [~@vars])
                                       ~g!s))))]
      (if ~n
          (islice (~g!res) 0 ~n)
          (~g!res)))))

(defmacro lazy-run* [vars &rest args]
  `(lazy-run None ~vars ~@args))

(defmacro run [n vars &rest args]
  `(list (lazy-run ~n ~vars ~@args)))

(defmacro-alias [run1 run¹] [vars &rest args]
  `(first (run 1 ~vars ~@args)))

(defmacro run* [vars &rest args]
  `(run None ~vars ~@args))

(defmacro/g! fresh [vars &rest goals]
  (if goals
      `(do
         (require [hy.contrib.walk [let :as ~g!let]])
         (~g!let [~@(--prep-fresh-vars-- vars)]
          (all ~@goals)))
    `succeed))

(defmacro/g! prep [&rest goals]
  (let [g!lvars (set [])]
    (prewalk (partial --prep-lvars-from-expr-- g!lvars) (HyList goals))
    (setv g!lvars (HyList g!lvars))
    `(fresh ~g!lvars ~@goals)))

(eval-and-compile
 (defn project-binding [s]
   (fn [var]
     `[~var (reify ~(HySymbol (+ "__" var)) ~s)]))

 (defn project-bindings [vars s]
   (setv bindings [])
   (for [var vars]
     (.extend bindings ((project-binding s) var)))
   bindings)

 (defn --prep-project-vars-- [vars]
   (setv project-vars [])
   (for [x vars]
     (.extend project-vars `[~(HySymbol (+ "__" x)) ~x]))
   project-vars))

(defmacro/g! project [vars &rest goals]
  (if goals
    `(fn [~g!s]
       (do
         (require [hy.contrib.walk [let :as ~g!let]])
         (~g!let [~@(--prep-project-vars-- vars)]
          (~g!let [~@(project-bindings vars g!s)]
           ((all ~@goals) ~g!s)))))
    `succeed))

(deftag U [n] `(unbound ~n))

;; Goals

(defn-alias [≡ == unifyo] [u v]
  (fn [s]
    (yield (unify u v s))))

(defn succeed [s]
  (yield s))
(setv s# succeed)

(defn fail [s]
  (iter (,)))
(setv u# fail)

(deftag ? [v] `(LVar (gensym '~v)))

(defn m-bind-sequence [mv f]
  (when mv
    (let [vs (list mv)]
         (chain (f (first vs))
                (m-bind-sequence (list (rest vs)) f)))))

(defmonad logic-m
  [m-result (fn [v] (list v))
   m-bind   m-bind-sequence
   m-zero   []
   m-plus   (fn [mvs]
              (chain #* mvs))])

(defmonad logic-interleave-m
  [m-result (fn [v] (list v))
   m-bind   m-bind-sequence
   m-zero   []
   m-plus   (fn [mvs]
              (interleave mvs))])

(defn all [&rest goals]
  (if goals
    (reduce (fn [g1 g2]
              (fn [s]
                (for [opt-s1 (g1 s)]
                  (unless (none? opt-s1)
                    (for [opt-s2 (g2 opt-s1)]
                      (yield opt-s2)))))) goals)
    succeed))

(defn-alias [allⁱ alli] [&rest goals]
  (if goals
    (with-monad logic-interleave-m
      (reduce (fn [chain-expr step]
                (fn [v]
                  (if (empty? v)
                    (step v)
                    (m-bind (chain-expr v) step))))
              goals
              m-result))
    succeed))

(eval-and-compile
 (defn __subst-else [conds]
   (map (fn [c]
          (if (= (first c) 'else)
              (HyExpression `(cons succeed ~(list (rest c))))
            c)) conds)))

(defmacro-alias [condᵉ conde] [&rest cs]
  (with-gensyms [s c with-monad]
    (let [ncs (__subst-else cs)]
         `(do
            (require [monaxhyd.core [with-monad :as ~with-monad]])
            (~with-monad logic-m
              (fn [~s]
                (m-plus (map (fn [~c]
                               ((all #* ~c) ~s))
                             [~@ncs]))))))))

(defmacro-alias [condⁱ condi] [&rest cs]
  (with-gensyms [s c with-monad]
    (let [ncs (__subst-else cs)]
         `(do
            (require [monaxhyd.core [with-monad :as ~with-monad]])
            (~with-monad logic-interleave-m
             (fn [~s]
               (m-plus (map (fn [~c]
                              ((all #* ~c) ~s))
                            [~@ncs]))))))))

(defn-alias [consᵒ conso] [f r l]
  (≡ (cons f r) l)
  ;; XXX: This is a rather limiting assumption relative to `cons` semantics.
  ;; Returning a list-pair like this says:
  ;; (conso f r l) == (== [f r] l) == (== (cons f '(r)) l)
  ;;
  ;; So, for `r` an LVar, we're implicitly forcing an interpretation of
  ;; `r` as a list, and, with otherwise consistent use of `cons`, it would
  ;; never unify with a plain symbol.
  ;;
  ;; Instead, we could use `conde` to consider both cases, no?
  #_(≡ [f r] l))

(defn-alias [firstᵒ firsto] [l a]
  (fresh [d]
         (≡ (cons a d) l)))

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
   [(pairᵒ l)
    (fresh [d]
           (restᵒ l d)
           (listᵒ d))]
   (else fail)))

(defn-alias [lolᵒ lolo] [l]
  (condᵉ
   [(emptyᵒ l) succeed]
   [(fresh [a]
           (firstᵒ l a)
           (listᵒ a))
    (fresh [d]
           (restᵒ l d)
           (lolᵒ d))]
   (else fail)))

(defn-alias [twinsᵒ twinso] [s]
  (fresh [x]
         (≡ [x x] s)))

(defn-alias [lotᵒ loto] [l]
  (listofᵒ twinsᵒ l))

(defn-alias [listofᵒ listofo] [predᵒ l]
  (condᵉ
   [(emptyᵒ l) succeed]
   [(fresh [a]
           (firstᵒ l a)
           (predᵒ a))
    (fresh [d]
           (restᵒ l d)
           (listofᵒ predᵒ d))]))

(defn-alias [memberᵒ membero] [x l]
  (condᵉ
   [(firstᵒ l x) succeed]
   (else (fresh [d]
                (restᵒ l d)
                (memberᵒ x d)))))

(defn-alias [pmemberᵒ pmembero] [x l]
  (condᵉ
   [(firstᵒ l x)
    (fresh [a d]
           (restᵒ l (cons a d)))]
   [(firstᵒ l x) (restᵒ l [])]
   (else (fresh [d]
                (restᵒ l d)
                (pmemberᵒ x d)))))

(defn-alias [memberrevᵒ memberrevo] [x l]
  (condᵉ
   [succeed (fresh [d]
               (restᵒ l d)
               (memberrevᵒ x d))]
   (else (firstᵒ l x))))

(defn-alias [memᵒ memo] [x l out]
  (condᵉ
   [(firstᵒ l x) (≡ l out)]
   [(fresh [d]
           (restᵒ l d)
           (memᵒ x d out))]))

(defn-alias [rememberᵒ remembero] [x l out]
  (condᵉ
   [(emptyᵒ l) (≡ [] out)]
   [(firstᵒ l x) (restᵒ l out)]
   [(fresh [a d res]
           (consᵒ a d l)
           (rememberᵒ x d res)
           (consᵒ a res out))]))

(defn-alias [appendᵒ appendo] [l s out]
  (condᵉ
   [(emptyᵒ l) (≡ s out)]
   [(fresh [a d res]
           (consᵒ a d l)
           (consᵒ a res out)
           (appendᵒ d s res))]))

(defn-alias [unwrapᵒ unwrapo] [x out]
  (condᵉ
   [succeed (≡ x out)]
   [(pairᵒ x)
    (fresh [a]
           (firstᵒ x a)
           (unwrapᵒ a out))]))

(defn-alias [flattenᵒ flatteno] [s out]
  (condᵉ
   [(emptyᵒ s) (≡ [] out)]
   [(pairᵒ s)
    (fresh [a d res-a res-d]
           (consᵒ a d s)
           (flattenᵒ a res-a)
           (flattenᵒ d res-d)
           (appendᵒ res-a res-d out))]
   (else (consᵒ s [] out))))

(defn-alias [flattenrevᵒ flattenrevo] [s out]
  (condᵉ
   [succeed (consᵒ s [] out)]
   [(emptyᵒ s) (≡ [] out)]
   [(pairᵒ s)
    (fresh [a d res-a res-d]
           (consᵒ a d s)
           (flattenrevᵒ a res-a)
           (flattenrevᵒ d res-d)
           (appendᵒ res-a res-d out))]))

(defn-alias [anyᵒ anyo] [g]
  (condᵉ
   [g succeed]
   (else (anyᵒ g))))

(setv neverᵒ (anyᵒ fail))
(setv nevero neverᵒ)

(setv alwaysᵒ (anyᵒ succeed))
(setv alwayso alwaysᵒ)

(defn-alias [salᵒ salo] [g]
  (condᵉ
   [succeed succeed]
   (else g)))
