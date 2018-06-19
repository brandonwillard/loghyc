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

(import [itertools [islice chain]]
        [functools [reduce partial]]
        [hy [HySymbol HyList HyKeyword]]
        [hy.contrib.walk [prewalk]])
(require [hy.contrib.walk [let]])
(require monaxhyd.core)

(import [adderall.internal [unify lvar? seq? reify LVar unbound interleave]])

;; (require hy.contrib.alias)
;; `hy.contrib.alias` was removed, so we simply add the provided functions.
(defmacro defmacro-alias [names lambda-list &rest body]
  "define one macro with several names"
  (setv ret `(do))
  (for* [name names]
    (.append ret
             `(defmacro ~name ~lambda-list ~@body)))
  ret)

(defmacro defn-alias [names lambda-list &rest body]
  "define one function with several names"
  (let [main (first names)
        aliases (rest names)]
       (setv ret `(do (defn ~main ~lambda-list ~@body)))
       (for* [name aliases]
         (.append ret
                  `(setv ~name ~main)))
       ret))

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

(defmacro lazy-run [n vars &rest goals]
  (with-gensyms [s res]
    `(let [~@(--prep-fresh-vars-- vars)
           ~res (fn [] (for [~s ((all ~@goals) (,))]
                         (when (nil? ~s)
                           (continue))
                         (yield (reify (if (= (len ~vars) 1)
                                         (first ~vars)
                                         [~@vars]) ~s))))]
       (if ~n
         (islice (~res) 0 ~n)
         (~res)))))

(defmacro lazy-run* [vars &rest args]
  `(lazy-run nil ~vars ~@args))

(defmacro run [n vars &rest args]
  `(list (lazy-run ~n ~vars ~@args)))

(defmacro-alias [run1 run¹] [vars &rest args]
  `(first (run 1 ~vars ~@args)))

(defmacro run* [vars &rest args]
  `(run nil ~vars ~@args))

(defmacro fresh [vars &rest goals]
  (if goals
    `(let [~@(--prep-fresh-vars-- vars)]
       (all ~@goals))
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
       (let [~@(--prep-project-vars-- vars)]
         (let [~@(project-bindings vars g!s)]
           ((all ~@goals) ~g!s))))
    `succeed))

(deftag U [n] `(unbound ~n))

;; Goals

(defn-alias [≡ == unifyo] [u v]
  (fn [s]
    (yield (unify u v s))))

(defn succeed [s]
  (yield s))
(deftag s [_] `succeed)

(defn fail [s]
  (iter ()))
(deftag u [_] `fail)

(deftag ? [v] `(LVar (gensym '~v)))

(defmonad logic-m
  [m-result (fn [v] (list v))
   m-bind   (defn m-bind-sequence [mv f]
              (when mv
                (let [vs (list mv)]
                  (chain (f (first vs))
                         (m-bind-sequence (rest vs) f)))))
   m-zero   []
   m-plus   (fn [mvs]
              (apply chain mvs))])

(defmonad logic-interleave-m
  [m-result (fn [v] (list v))
   m-bind   (defn m-bind-sequence [mv f]
              (when mv
                (let [vs (list mv)]
                  (chain (f (first vs))
                         (m-bind-sequence (rest vs) f)))))
   m-zero   []
   m-plus   (fn [mvs]
              (interleave mvs))])

(defn all [&rest goals]
  (if goals
    (reduce (fn [g1 g2]
              (fn [s]
                (for [opt-s1 (g1 s)]
                  (unless (nil? opt-s1)
                    (for [opt-s2 (g2 opt-s1)]
                      (yield opt-s2)))))) goals)
    succeed))

(defn-alias [allⁱ alli] [&rest goals]
  (if goals
    (with-monad logic-interleave-m
      (reduce (defn m-chain-link [chain-expr step]
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
            (HyList `(#s . ~(rest c)))
            c)) conds)))

(defmacro-alias [condᵉ conde] [&rest cs]
  (with-gensyms [s c]
    (let [ncs (__subst-else cs)]
      `(with-monad logic-m
         (fn [~s]
           (m-plus (map (fn [~c]
                          ((apply all ~c) ~s))
                        [~@ncs])))))))


(defmacro-alias [condⁱ condi] [&rest cs]
  (with-gensyms [s c]
    (let [ncs (__subst-else cs)]
      `(with-monad logic-interleave-m
         (fn [~s]
           (m-plus (map (fn [~c]
                          ((apply all ~c) ~s))
                        [~@ncs])))))))

(defn-alias [consᵒ conso] [f r l]
  (cond
   [(or (nil? r) (= r [])) (≡ [f] l)]
   [(or (lvar? r) (seq? r)) (≡ (cons f r) l)]
   [true (≡ (cons f r) l)]))

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
   [(emptyᵒ l) #s]
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
   [(emptyᵒ l) #s]
   [(fresh [a]
           (firstᵒ l a)
           (predᵒ a))
    (fresh [d]
           (restᵒ l d)
           (listofᵒ predᵒ d))]))

(defn-alias [memberᵒ membero] [x l]
  (condᵉ
   [(firstᵒ l x) #s]
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
   [#s (fresh [d]
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
   [#s (≡ x out)]
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
   [#s (consᵒ s [] out)]
   [(emptyᵒ s) (≡ [] out)]
   [(pairᵒ s)
    (fresh [a d res-a res-d]
           (consᵒ a d s)
           (flattenrevᵒ a res-a)
           (flattenrevᵒ d res-d)
           (appendᵒ res-a res-d out))]))

(defn-alias [anyᵒ anyo] [g]
  (condᵉ
   [g #s]
   (else (anyᵒ g))))

(def neverᵒ (anyᵒ #u))
(def nevero neverᵒ)

(def alwaysᵒ (anyᵒ #s))
(def alwayso alwaysᵒ)

(defn-alias [salᵒ salo] [g]
  (condᵉ
   [#s #s]
   (else g)))
