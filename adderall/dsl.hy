;; adderall - miniKanren in Hy
;; Copyright (C) 2014  Gergely Nagy <algernon@madhouse-project.org>
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
        [adderall.internal [unify lvar? seq? reify LVar unbound interleave]]
        [hy [HySymbol HyList HyKeyword]]
        [hy.contrib.walk [prewalk]])
(require monaxhyd.core)

;; Top level stuff

(eval-and-compile
 (defn --prep-fresh-vars-- [vars]
   (list-comp `[~x (LVar (gensym '~x))] [x vars]))

 (defn --prep-lvars-from-expr-- [lvars expr]
   (when (and (instance? HySymbol expr)
              (.startswith expr "?"))
     (.add lvars expr))
   expr))

(defmacro lazy-run [n vars &rest goals]
  (with-gensyms [s res]
    `(let [~@(--prep-fresh-vars-- vars)
           [~res (fn [] (for [~s ((all ~@goals) (,))]
                         (when (nil? ~s)
                           (continue))
                         (yield (reify (if (= (len ~vars) 1)
                                         (first ~vars)
                                         [~@vars]) ~s))))]]
       (if ~n
         (islice (~res) 0 ~n)
         (~res)))))

(defmacro run [n vars-or-variant &rest args]
  (if (= (type vars-or-variant) HyKeyword)
    (if (= vars-or-variant :lazy)
      `(lazy-run ~n ~@args)
      `(list (lazy-run ~n ~@args)))
    `(list (lazy-run ~n ~vars-or-variant ~@args))))

(defmacro run* [vars-or-variant &rest args]
  `(run nil ~vars-or-variant ~@args))

(defmacro fresh [vars &rest goals]
  (if goals
    `(let [~@(--prep-fresh-vars-- vars)]
        (all ~@goals))
    `succeed))

(defmacro prep [&rest goals]
  (let [[lvars (set [])]]
    (prewalk (partial --prep-lvars-from-expr-- lvars) (HyList goals))
    (setv lvars (HyList lvars))
    `(fresh ~lvars ~@goals)))

(eval-and-compile
 (defn project-binding [s]
   (fn [var]
     `[[~var (reify ~(HySymbol (+ "__" var)) ~s)]]))

 (defn project-bindings [vars s]
   (reduce chain (map (project-binding s) vars))))

(defmacro/g! project [vars &rest goals]
  (if goals
    `(fn [~g!s]
       (let [~@(list-comp `[~(HySymbol (+ "__" x)) ~x] [x vars])]
         (let [~@(project-bindings vars g!s)]
           ((all ~@goals) ~g!s))))
    `succeed))

(defreader U [n] `(unbound ~n))

;; Goals

(defn-alias [≡ == unifyo] [u v]
  (fn [s]
    (yield (unify u v s))))

(defn succeed [s]
  (yield s))
(defreader s [_] `succeed)

(defn fail [s]
  (iter ()))
(defreader u [_] `fail)

(defreader ? [v] `(LVar (gensym '~v)))

(defmonad logic-m
  [[m-result (fn [v] (list v))]
   [m-bind   (defn m-bind-sequence [mv f]
               (when mv
                 (let [[vs (list mv)]]
                   (chain (f (first vs))
                          (m-bind-sequence (rest vs) f)))))]
   [m-zero   []]
   [m-plus   (fn [mvs]
               (apply chain mvs))]])

(defmonad logic-interleave-m
  [[m-result (fn [v] (list v))]
   [m-bind   (defn m-bind-sequence [mv f]
               (when mv
                 (let [[vs (list mv)]]
                   (chain (f (first vs))
                          (m-bind-sequence (rest vs) f)))))]
   [m-zero   []]
   [m-plus   (fn [mvs]
               (interleave mvs))]])

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
            (HyList `(#ss . ~(rest c)))
            c)) conds)))

(defmacro-alias [condᵉ conde] [&rest cs]
  (with-gensyms [s c]
    (let [[ncs (__subst-else cs)]]
      `(with-monad logic-m
         (fn [~s]
           (m-plus (map (fn [~c]
                          ((apply all ~c) ~s))
                        [~@ncs])))))))


(defmacro-alias [condⁱ condi] [&rest cs]
  (with-gensyms [s c]
    (let [[ncs (__subst-else cs)]]
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
   [#ss (fresh [d]
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
   [#ss (≡ x out)]
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
   [#ss (consᵒ s [] out)]
   [(emptyᵒ s) (≡ [] out)]
   [(pairᵒ s)
    (fresh [a d res-a res-d]
           (consᵒ a d s)
           (flattenrevᵒ a res-a)
           (flattenrevᵒ d res-d)
           (appendᵒ res-a res-d out))]))

(defn-alias [anyᵒ anyo] [g]
  (condᵉ
   [g #ss]
   (else (anyᵒ g))))

(def neverᵒ (anyᵒ #uu))
(def nevero neverᵒ)

(def alwaysᵒ (anyᵒ #ss))
(def alwayso alwaysᵒ)

(defn-alias [salᵒ salo] [g]
  (condᵉ
   [#ss #ss]
   (else g)))
