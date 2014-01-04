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

(import [adderall.dsl [*]]
        [hy.models.symbol [HySymbol]])
(require adderall.dsl)

(defn unbound [n]
  (LVar (.format "_.{0}" n)))

(defmacro frame [frame-num value expr]
  (let [[res (gensym)]
        [name (+ 'test-rs- frame-num)]]
    `(defn ~(HySymbol name) []
       (let [[q (fresh [q])]
             [~res ~expr]]
         (assert (= ~value ~res))))))

(frame "1.10" []
       (run* q
             #uu))

(frame "1.11" [True]
       (run* q
             (=ᵒ True q)))

(frame "1.12" []
       (run* q
             #uu
             (=ᵒ True q)))

(frame "1.13" [True]
       (run* q
             #ss
             (=ᵒ True q)))

(frame "1.15" ['corn]
       (run* q
             #ss
             (=ᵒ 'corn q)))

(frame "1.17" []
       (run* q
             #uu
             (=ᵒ 'corn q)))

(frame "1.18" [False]
       (run* q
             #ss
             (=ᵒ False q)))

(frame "1.22" []
       (run* q
             (let [[x False]]
               (=ᵒ True x))))

(frame "1.23" [True]
       (run* q
             (fresh [x]
                    (=ᵒ True x)
                    (=ᵒ True q))))

(frame "1.26" [True]
       (run* q
             (fresh [x]
                    (=ᵒ x True)
                    (=ᵒ True q))))

(frame "1.26" [True]
       (run* q
             (fresh [x]
                    (=ᵒ x True)
                    (=ᵒ q True))))

(frame "1.28" [(unbound 0)]
       (run* q
             #ss))

(frame "1.29" [(unbound 0)]
       (run* q
             (let [[q False]]
               (fresh [q]
                      (=ᵒ True q)))))

(frame "1.30" [(, (unbound 0) (unbound 1))]
       (run* q
             (fresh [x y]
                    (=ᵒ (, x y) q))))

(frame "1.31" [(, (unbound 0) (unbound 1))]
       (run* q
             (fresh [t u]
                    (=ᵒ (, t u) q))))

(frame "1.32" [(, (unbound 0) (unbound 1) (unbound 0))]
       (run* q
             (fresh [x]
                    (let [[y x]]
                      (fresh [x]
                             (=ᵒ (, y x y) q))))))

(frame "1.33" [(, (unbound 0) (unbound 1) (unbound 0))]
       (run* q
             (fresh [x]
                    (let [[y x]]
                      (fresh [x]
                             (=ᵒ (, x y x) q))))))

(frame "1.34" []
       (run* q
             (=ᵒ False q)
             (=ᵒ True q)))

(frame "1.35" [False]
       (run* q
             (=ᵒ False q)
             (=ᵒ False q)))

(frame "1.36" [True]
       (run* q
             (let [[x q]]
               (=ᵒ True x))))

(frame "1.37" [(unbound 0)]
       (run* q
             (fresh [x]
                    (=ᵒ x q))))

(frame "1.38" [True]
       (run* q
             (fresh [x]
                    (=ᵒ True x)
                    (=ᵒ x q))))

(frame "1.39" [True]
       (run* q
             (fresh [x]
                    (=ᵒ x q)
                    (=ᵒ True x))))

(frame "1.47" [:olive :oil]
       (run* q
             (condᵉ
              [(=ᵒ :olive q) #ss]
              [(=ᵒ :oil q) #ss]
              [#uu])))

(frame "1.49" [:olive]
       (run 1 q
            (condᵉ
             [(=ᵒ :olive q) #ss]
             [(=ᵒ :oil q) #ss]
             [#uu])))

(frame "1.50" [:olive (unbound 0) :oil]
       (run* q
             (condᵉ
              [(=ᵒ :virgin q) #uu]
              [(=ᵒ :olive q) #ss]
              [#ss #ss]
              [(=ᵒ :oil q) #ss]
              [#uu])))

(frame "1.52" [:extra :olive]
       (run 2 q
            (condᵉ
             [(=ᵒ :extra q) #ss]
             [(=ᵒ :virgin q) #uu]
             [(=ᵒ :olive q) #ss]
             [(=ᵒ :oil q) #ss]
             [#uu])))

(frame "1.53" [(, :split :pea)]
       (run* q
             (fresh [x y]
                    (=ᵒ :split x)
                    (=ᵒ :pea y)
                    (=ᵒ (, x y) q))))

(frame "1.54" [(, :split :pea) (, :navy :bean)]
       (run* q
             (fresh [x y]
                    (condᵉ
                     [(=ᵒ :split x) (=ᵒ :pea y)]
                     [(=ᵒ :navy x) (=ᵒ :bean y)]
                     [#uu])
                    (=ᵒ (, x y) q))))

(frame "1.55" [(, :split :pea :soup) (, :navy :bean :soup)]
       (run* q
             (fresh [x y]
                    (condᵉ
                     [(=ᵒ :split x) (=ᵒ :pea y)]
                     [(=ᵒ :navy x) (=ᵒ :bean y)]
                     [#uu])
                    (=ᵒ (, x y :soup) q))))

(defn teacupᵒ [x]
  (condᵉ
   [(=ᵒ :tea x) #ss]
   [(=ᵒ :cup x) #ss]
   [#uu]))

(frame "1.56" [:tea :cup]
       (run* q
             (teacupᵒ q)))

;; NOTE: This was modified, because our evaluation order differs from
;; that of the Reasoned Schemer. In TRS, (, False True) comes last.
(frame "1.57" [(, :tea True) (, False True) (, :cup True)]
       (run* q
             (fresh [x y]
                    (condᵉ
                     [(teacupᵒ x) (=ᵒ True y) #ss]
                     [(=ᵒ False x) (=ᵒ True y)]
                     [#uu])
                    (=ᵒ (, x y) q))))

(frame "1.58" [(, (unbound 0) (unbound 1))
               (, (unbound 0) (unbound 1))]
       (run* q
             (fresh [x y z]
                    (condᵉ
                     [(=ᵒ y x) (fresh [x] (=ᵒ z x))]
                     [(fresh [x] (=ᵒ y x)) (=ᵒ z x)]
                     [#uu])
                    (=ᵒ (, y z) q))))

(frame "1.59" [(, False (unbound 0)) (, (unbound 0) False)]
       (run* q
             (fresh [x y z]
                    (condᵉ
                     [(=ᵒ y x) (fresh [x] (=ᵒ z x))]
                     [(fresh [x] (=ᵒ y x)) (=ᵒ z x)]
                     [#uu])
                    (=ᵒ False x)
                    (=ᵒ (, y z) q))))
(frame "1.60" [False]
       (run* q
             (let [[a (=ᵒ True q)]
                   [b (=ᵒ False q)]]
               b)))

(frame "1.61" [False]
       (run* q
             (let [[a (=ᵒ True q)]
                   [b (fresh [x]
                             (=ᵒ x q)
                             (=ᵒ False x))]
                   [c (condᵉ
                       [(=ᵒ True q) #ss]
                       [(=ᵒ False q)])]]
               b)))
