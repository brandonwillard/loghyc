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
        [tests.schemer.common [*]])
(require adderall.dsl)
(require tests.schemer.common)

(frame "1.10" []
       (run* [q]
             #uu))

(frame "1.11" [true]
       (run* [q]
             (≡ true q)))

(frame "1.12" []
       (run* [q]
             #uu
             (≡ true q)))

(frame "1.13" [true]
       (run* [q]
             #ss
             (≡ true q)))

(frame "1.16" ['corn]
       (run* [r]
             #ss
             (≡ 'corn r)))

(frame "1.17" []
       (run* [r]
             #uu
             (≡ 'corn r)))

(frame "1.18" [false]
       (run* [q]
             #ss
             (≡ false q)))

(frame "1.22" []
       (run* [x]
             (let [[x false]]
               (≡ true x))))

(frame "1.23" [true]
       (run* [q]
             (fresh [x]
                    (≡ true x)
                    (≡ true q))))

(frame "1.26" [true]
       (run* [q]
             (fresh [x]
                    (≡ x true)
                    (≡ true q))))

(frame "1.27" [true]
       (run* [q]
             (fresh [x]
                    (≡ x true)
                    (≡ q true))))

(frame "1.28" [(unbound 0)]
       (run* [x]
             #ss))

(frame "1.29" [(unbound 0)]
       (run* [x]
             (let [[x false]]
               (fresh [x]
                      (≡ true x)))))

(frame "1.30" [[(unbound 0) (unbound 1)]]
       (run* [r]
             (fresh [x y]
                    (≡ (cons x (cons y ())) r))))

(frame "1.31" [[(unbound 0) (unbound 1)]]
       (run* [s]
             (fresh [t u]
                    (≡ (cons t (cons u ())) s))))

(frame "1.32" [[(unbound 0) (unbound 1) (unbound 0)]]
       (run* [r]
             (fresh [x]
                    (let [[y x]]
                      (fresh [x]
                             (≡ (cons y (cons x (cons y ()))) r))))))

(frame "1.33" [[(unbound 0) (unbound 1) (unbound 0)]]
       (run* [r]
             (fresh [x]
                    (let [[y x]]
                      (fresh [x]
                             (≡ (cons x (cons y (cons x ()))) r))))))

(frame "1.34" []
       (run* [q]
             (≡ false q)
             (≡ true q)))

(frame "1.35" [false]
       (run* [q]
             (≡ false q)
             (≡ false q)))

(frame "1.36" [true]
       (run* [q]
             (let [[x q]]
               (≡ true x))))

(frame "1.37" [(unbound 0)]
       (run* [r]
             (fresh [x]
                    (≡ x r))))

(frame "1.38" [true]
       (run* [q]
             (fresh [x]
                    (≡ true x)
                    (≡ x q))))

(frame "1.39" [true]
       (run* [q]
             (fresh [x]
                    (≡ x q)
                    (≡ true x))))

(frame "1.47" [:olive :oil]
       (run* [x]
             (condᵉ
              [(≡ :olive x) #ss]
              [(≡ :oil x) #ss]
              [#uu])))

(frame "1.49" [:olive]
       (run 1 [x]
            (condᵉ
             [(≡ :olive x) #ss]
             [(≡ :oil x) #ss]
             [#uu])))

(frame "1.50" [:olive (unbound 0) :oil]
       (run* [x]
             (condᵉ
              [(≡ :virgin x) #uu]
              [(≡ :olive x) #ss]
              [#ss #ss]
              [(≡ :oil x) #ss]
              [#uu])))

(frame "1.52" [:extra :olive]
       (run 2 [x]
            (condᵉ
             [(≡ :extra x) #ss]
             [(≡ :virgin x) #uu]
             [(≡ :olive x) #ss]
             [(≡ :oil x) #ss]
             [#uu])))

(frame "1.53" [[:split :pea]]
       (run* [r]
             (fresh [x y]
                    (≡ :split x)
                    (≡ :pea y)
                    (≡ (cons x (cons y ())) r))))

(frame "1.54" [[:split :pea] [:navy :bean]]
       (run* [r]
             (fresh [x y]
                    (condᵉ
                     [(≡ :split x) (≡ :pea y)]
                     [(≡ :navy x) (≡ :bean y)]
                     [#uu])
                    (≡ (cons x (cons y ())) r))))

(frame "1.55" [[:split :pea :soup] [:navy :bean :soup]]
       (run* [r]
             (fresh [x y]
                    (condᵉ
                     [(≡ :split x) (≡ :pea y)]
                     [(≡ :navy x) (≡ :bean y)]
                     [#uu])
                    (≡ (cons x (cons y (cons :soup ()))) r))))

(defn teacupᵒ [x]
  (condᵉ
   [(≡ :tea x) #ss]
   [(≡ :cup x) #ss]
   [#uu]))

(frame "1.56" [:tea :cup]
       (run* [x]
             (teacupᵒ x)))

;; NOTE: This was modified, because our evaluation order differs from
;; that of the Reasoned Schemer. In TRS, (, false true) comes last.
(frame "1.57" [[:tea true] [false true] [:cup true]]
       (run* [r]
             (fresh [x y]
                    (condᵉ
                     [(teacupᵒ x) (≡ true y) #ss]
                     [(≡ false x) (≡ true y)]
                     [#uu])
                    (≡ (cons x (cons y ())) r))))

(frame "1.58" [[(unbound 0) (unbound 1)]
               [(unbound 0) (unbound 1)]]
       (run* [r]
             (fresh [x y z]
                    (condᵉ
                     [(≡ y x) (fresh [x] (≡ z x))]
                     [(fresh [x] (≡ y x)) (≡ z x)]
                     [#uu])
                    (≡ (cons y (cons z ())) r))))

(frame "1.59" [[false (unbound 0)] [(unbound 0) false]]
       (run* [r]
             (fresh [x y z]
                    (condᵉ
                     [(≡ y x) (fresh [x] (≡ z x))]
                     [(fresh [x] (≡ y x)) (≡ z x)]
                     [#uu])
                    (≡ false x)
                    (≡ (cons y (cons z ())) r))))

(frame "1.60" [false]
       (run* [q]
             (let [[a (≡ true q)]
                   [b (≡ false q)]]
               b)))

(frame "1.61" [false]
       (run* [q]
             (let [[a (≡ true q)]
                   [b (fresh [x]
                             (≡ x q)
                             (≡ false x))]
                   [c (condᵉ
                       [(≡ true q) #ss]
                       [(≡ false q)])]]
               b)))
