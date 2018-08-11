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

(import [adderall.dsl [*]])
(import [tests.schemer.common [*]])

(require [hy.contrib.walk [let]])
(require [adderall.dsl [*]])
(require [tests.schemer.common [*]])


(frame "1.10" []
       (run* [q]
             fail))

(frame "1.11" [True]
       (run* [q]
             (≡ True q)))

(frame "1.12" []
       (run* [q]
             fail
             (≡ True q)))

(frame "1.13" [True]
       (run* [q]
             succeed
             (≡ True q)))

(frame "1.16" ['corn]
       (run* [r]
             succeed
             (≡ 'corn r)))

(frame "1.17" []
       (run* [r]
             fail
             (≡ 'corn r)))

(frame "1.18" [False]
       (run* [q]
             succeed
             (≡ False q)))

(frame "1.22" []
       (run* [x]
             (let [x False]
               (≡ True x))))

(frame "1.23" [True]
       (run* [q]
             (fresh [x]
                    (≡ True x)
                    (≡ True q))))

(frame "1.26" [True]
       (run* [q]
             (fresh [x]
                    (≡ x True)
                    (≡ True q))))

(frame "1.27" [True]
       (run* [q]
             (fresh [x]
                    (≡ x True)
                    (≡ q True))))

(frame "1.28" [#U 0]
       (run* [x]
             succeed))

(frame "1.29" [#U 0]
       (run* [x]
             (let [x False]
               (fresh [x]
                      (≡ True x)))))

(frame "1.30" [[#U 0 #U 1]]
       (run* [r]
             (fresh [x y]
                    ;; Original is `(cons x (cons y '()))`, but `cons` is different here.
                    (≡ [x y] r))))

(frame "1.31" [[#U 0 #U 1]]
       (run* [s]
             (fresh [t u]
                    ;; Original is `(cons t (cons u '()))`, but `cons` is different here.
                    (≡ [t u] s))))

(frame "1.32" [[#U 0 #U 1 #U 0]]
       (run* [r]
             (fresh [x]
                    (let [y x]
                      (fresh [x]
                             ;; Original is `(cons y (cons x (cons x '())))`, but `cons` is different here.
                             (≡ [y x y] r))))))

(frame "1.33" [[#U 0 #U 1 #U 0]]
       (run* [r]
             (fresh [x]
                    (let [y x]
                      (fresh [x]
                             ;; Original is `(cons x (cons y (cons y '())))`, but `cons` is different here.
                             (≡ [x y x] r))))))

(frame "1.34" []
       (run* [q]
             (≡ False q)
             (≡ True q)))

(frame "1.35" [False]
       (run* [q]
             (≡ False q)
             (≡ False q)))

(frame "1.36" [True]
       (run* [q]
             (let [x q]
               (≡ True x))))

(frame "1.37" [#U 0]
       (run* [r]
             (fresh [x]
                    (≡ x r))))

(frame "1.38" [True]
       (run* [q]
             (fresh [x]
                    (≡ True x)
                    (≡ x q))))

(frame "1.39" [True]
       (run* [q]
             (fresh [x]
                    (≡ x q)
                    (≡ True x))))

(frame "1.47" ['olive 'oil]
       (run* [x]
             (condᵉ
              [(≡ 'olive x) succeed]
              [(≡ 'oil x) succeed]
              (else fail))))

(frame "1.49" ['olive]
       (run 1 [x]
            (condᵉ
             [(≡ 'olive x) succeed]
             [(≡ 'oil x) succeed]
             (else fail))))

(frame "1.50" ['olive #U 0 'oil]
       (run* [x]
             (condᵉ
              [(≡ 'virgin x) fail]
              [(≡ 'olive x) succeed]
              [succeed succeed]
              [(≡ 'oil x) succeed]
              (else fail))))

(frame "1.52" ['extra 'olive]
       (run 2 [x]
            (condᵉ
             [(≡ 'extra x) succeed]
             [(≡ 'virgin x) fail]
             [(≡ 'olive x) succeed]
             [(≡ 'oil x) succeed]
             (else fail))))

(frame "1.53" [['split 'pea]]
       (run* [r]
             (fresh [x y]
                    (≡ 'split x)
                    (≡ 'pea y)
                    ;; Original is `(cons x (cons y '()))`, but `cons` is different here.
                    (≡ [x y] r))))

(frame "1.54" [['split 'pea] ['navy 'bean]]
       (run* [r]
             (fresh [x y]
                    (condᵉ
                     [(≡ 'split x) (≡ 'pea y)]
                     [(≡ 'navy x) (≡ 'bean y)]
                     (else fail))
                    ;; Original is `(cons x (cons y '()))`, but `cons` is different here.
                    (≡ [x y] r))))

(frame "1.55" [['split 'pea 'soup] ['navy 'bean 'soup]]
       (run* [r]
             (fresh [x y]
                    (condᵉ
                     [(≡ 'split x) (≡ 'pea y)]
                     [(≡ 'navy x) (≡ 'bean y)]
                     (else fail))
                    ;; Original is `(cons x (cons y (cons 'soup '())))`, but `cons` is different here.
                    (≡ [x y 'soup] r))))

(defn teacupᵒ [x]
  (condᵉ
   [(≡ 'tea x) succeed]
   [(≡ 'cup x) succeed]
   (else fail)))

(frame "1.56" ['tea 'cup]
       (run* [x]
             (teacupᵒ x)))

(frame "1.57" [['tea True] ['cup True] [False True]]
       (run* [r]
             (fresh [x y]
                    (condᵉ
                     [(teacupᵒ x) (≡ True y) succeed]
                     [(≡ False x) (≡ True y)]
                     (else fail))
                    ;; Original is `(cons x (cons y '()))`, but `cons` is different here.
                    (≡ [x y] r))))

(frame "1.58" [[#U 0 #U 1]
               [#U 0 #U 1]]
       (run* [r]
             (fresh [x y z]
                    (condᵉ
                     [(≡ y x) (fresh [x] (≡ z x))]
                     [(fresh [x] (≡ y x)) (≡ z x)]
                     (else fail))
                    ;; Original is `(cons y (cons z '()))`, but `cons` is different here.
                    (≡ [y z] r))))

(frame "1.59" [[False #U 0] [#U 0 False]]
       (run* [r]
             (fresh [x y z]
                    (condᵉ
                     [(≡ y x) (fresh [x] (≡ z x))]
                     [(fresh [x] (≡ y x)) (≡ z x)]
                     (else fail))
                    (≡ False x)
                    ;; Original is `(cons y (cons z '()))`, but `cons` is different here.
                    (≡ [y z] r))))

(frame "1.60" [False]
       (run* [q]
             (let [a (≡ True q)
                   b (≡ False q)]
               b)))

(frame "1.61" [False]
       (run* [q]
             (let [a (≡ True q)
                   b (fresh [x]
                            (≡ x q)
                            (≡ False x))
                   c (condᵉ
                      [(≡ True q) succeed]
                      [(≡ False q)])]
               b)))
