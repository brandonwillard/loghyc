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
(require adderall.lvar)
(require tests.schemer.common)

(frame "5.10" [[:cake :tastes :yummy]]
       (run* [x]
             (appendᵒ [:cake]
                      [:tastes :yummy]
                      x)))

(frame "5.11" [[:cake :with :ice #U0 :tastes :yummy]]
       (run* [x]
             (fresh [y]
                    (appendᵒ [:cake :with :ice y]
                             [:tastes :yummy]
                             x))))

(frame "5.12" [(list* :cake :with :ice :cream #U0)]
       (run* [x]
             (fresh [y]
                    (appendᵒ [:cake :with :ice :cream]
                             y
                             x))))

(frame "5.13" [[:cake :with :ice :d :t]]
       (run 1 [x]
            (fresh [y]
                   (appendᵒ (list* :cake :with :ice y)
                            [:d :t]
                            x))))

(frame "5.14" [[]]
       (run 1 [y]
            (fresh [x]
                   (appendᵒ (list* :cake :with :ice y)
                            [:d :t]
                            x))))


(frame "5.16" [[:cake :with :ice :d :t]
               [:cake :with :ice #U0 :d :t]
               [:cake :with :ice #U0 #U1 :d :t]
               [:cake :with :ice #U0 #U1 #U2 :d :t]
               [:cake :with :ice #U0 #U1 #U2 #U3 :d :t]]
       (run 5 [x]
            (fresh [y]
                   (appendᵒ (list* :cake :with :ice y)
                            [:d :t]
                            x))))

(frame "5.17" [[]
               [#U0]
               [#U0 #U1]
               [#U0 #U1 #U2]
               [#U0 #U1 #U2 #U3]]
       (run 5 [y]
            (fresh [x]
                   (appendᵒ (list* :cake :with :ice y)
                            [:d :t]
                            x))))

(frame "5.20" [[:cake :with :ice :d :t]
               [:cake :with :ice #U0 :d :t #U0]
               [:cake :with :ice #U0 #U1 :d :t #U0 #U1]
               [:cake :with :ice #U0 #U1 #U2 :d :t #U0 #U1 #U2]
               [:cake :with :ice #U0 #U1 #U2 #U3 :d :t #U0 #U1 #U2 #U3]]
       (run 5 [x]
            (fresh [y]
                   (appendᵒ (list* :cake :with :ice y)
                            (list* :d :t y)
                            x))))

(frame "5.21" [(list* :cake :with :ice :cream :d :t #U0)]
       (run* [x]
             (fresh [z]
                    (appendᵒ [:cake :with :ice :cream]
                             (list* :d :t z)
                             x))))

(frame "5.23" [[]
               [:cake]
               [:cake :with]
               [:cake :with :ice]
               [:cake :with :ice :d]
               [:cake :with :ice :d :t]]
       (run 6 [x]
            (fresh [y]
                   (appendᵒ x y [:cake :with :ice :d :t]))))

(frame "5.25" [[:cake :with :ice :d :t]
               [:with :ice :d :t]
               [:ice :d :t]
               [:d :t]
               [:t]
               []]
       (run 6 [y]
            (fresh [x]
                   (appendᵒ x y [:cake :with :ice :d :t]))))

(frame "5.27" [[[] [:cake :with :ice :d :t]]
               [[:cake] [:with :ice :d :t]]
               [[:cake :with] [:ice :d :t]]
               [[:cake :with :ice] [:d :t]]
               [[:cake :with :ice :d] [:t]]
               [[:cake :with :ice :d :t] []]]
       (run 6 [r]
            (fresh [x y]
                   (appendᵒ x y [:cake :with :ice :d :t])
                   (≡ [x y] r))))

(frame "5.32" [[[] [:cake :with :ice :d :t]]
               [[:cake] [:with :ice :d :t]]
               [[:cake :with] [:ice :d :t]]
               [[:cake :with :ice] [:d :t]]
               [[:cake :with :ice :d] [:t]]
               [[:cake :with :ice :d :t] []]]
       (run 7 [r]
            (fresh [x y]
                   (appendᵒ x y [:cake :with :ice :d :t])
                   (≡ [x y] r))))

(frame "5.33" [[]
               [#U0]
               [#U0 #U1]
               [#U0 #U1 #U2]
               [#U0 #U1 #U2 #U3]
               [#U0 #U1 #U2 #U3 #U4]
               [#U0 #U1 #U2 #U3 #U4 #U5]]
       (run 7 [x]
            (fresh [y z]
                   (appendᵒ x y z))))

(frame "5.34" [#U0 #U0 #U0 #U0 #U0 #U0 #U0]
       (run 7 [y]
            (fresh [x z]
                   (appendᵒ x y z))))

(frame "5.36" [#U0
               (list* #U0 #U1)
               (list* #U0 #U1 #U2)
               (list* #U0 #U1 #U2 #U3)
               (list* #U0 #U1 #U2 #U3 #U4)
               (list* #U0 #U1 #U2 #U3 #U4 #U5)
               (list* #U0 #U1 #U2 #U3 #U4 #U5 #U6)]
       (run 7 [z]
            (fresh [x y]
                   (appendᵒ x y z))))

(frame "5.37" [[[] #U0 #U0]
               [[#U0] #U1 (list* #U0 #U1)]
               [[#U0 #U1] #U2 (list* #U0 #U1 #U2)]
               [[#U0 #U1 #U2] #U3 (list* #U0 #U1 #U2 #U3)]
               [[#U0 #U1 #U2 #U3] #U4 (list* #U0 #U1 #U2 #U3 #U4)]
               [[#U0 #U1 #U2 #U3 #U4] #U5
                (list* #U0 #U1 #U2 #U3 #U4 #U5)]
               [[#U0 #U1 #U2 #U3 #U4 #U5] #U6
                (list* #U0 #U1 #U2 #U3 #U4 #U5 #U6)]]
       (run 7 [r]
            (fresh [x y z]
                   (appendᵒ x y z)
                   (≡ [x y z] r))))

;; The order is different here than in The Reasoned Schemer, because
;; in frame 5.52, we swap the conditions, which reverses the order.
(frame "5.46" [[[[:pizza]]]
               [[:pizza]]
               [:pizza]
               :pizza]
       (run* [x]
             (unwrapᵒ [[[:pizza]]] x)))

(frame "5.53" [:pizza
               (cons :pizza #U0)
               (cons (cons :pizza #U0) #U1)
               (cons (cons (cons :pizza #U0) #U1) #U2)
               (cons (cons (cons (cons :pizza #U0) #U1) #U2) #U3)]
       (run 5 [x]
            (unwrapᵒ x :pizza)))

(frame "5.54" [[[:pizza]]
               (cons [[:pizza]] #U0)
               (cons (cons [[:pizza]] #U0) #U1)
               (cons (cons (cons [[:pizza]] #U0) #U1) #U2)
               (cons (cons (cons (cons [[:pizza]] #U0) #U1) #U2) #U3)]
       (run 5 [x]
            (unwrapᵒ x [[:pizza]])))

(frame "5.55" [:pizza
               (cons :pizza #U0)
               (cons (cons :pizza #U0) #U1)
               (cons (cons (cons :pizza #U0) #U1) #U2)
               (cons (cons (cons (cons :pizza #U0) #U1) #U2) #U3)]
       (run 5 [x]
            (unwrapᵒ [[x]] :pizza)))

(frame "5.60" [[:a :b :c]]
       (run 1 [x]
            (flattenᵒ [[:a :b] :c] x)))

(frame "5.61" [[:a :b :c]]
       (run 1 [x]
            (flattenᵒ [:a [:b :c]] x)))

(frame "5.62" [[:a]
               [:a []]
               [[:a]]]
       (run* [x]
             (flattenᵒ [:a] x)))

(frame "5.64" [[:a]
               [:a []]
               [:a []]
               [:a [] []]
               [[:a]]
               [[:a] []]
               [[[:a]]]]
       (run* [x]
             (flattenᵒ [[:a]] x)))

(frame "5.66" [[:a]
               [:a []]
               [:a []]
               [:a [] []]
               [:a []]
               [:a [] []]
               [:a [] []]
               [:a [] [] []]
               [[:a]]
               [[:a] []]
               [[:a] []]
               [[:a] [] []]
               [[[:a]]]
               [[[:a]] []]
               [[[[:a]]]]]
       (run* [x]
             (flattenᵒ [[[:a]]] x)))

(frame "5.68" [[:a :b :c]
               [:a :b :c []]
               [:a :b [:c]]
               [:a :b [] :c]
               [:a :b [] :c []]
               [:a :b [] [:c]]
               [:a [:b] :c]
               [:a [:b] :c []]
               [:a [:b] [:c]]
               [[:a :b] :c]
               [[:a :b] :c []]
               [[:a :b] [:c]]
               [[[:a :b] :c]]]
       (run* [x]
             (flattenᵒ [[:a :b] :c] x)))

(frame "5.75" [[[[:a :b] :c]]
               [[:a :b] [:c]]
               [[:a :b] :c []]
               [[:a :b] :c]
               [:a [:b] [:c]]
               [:a [:b] :c []]
               [:a [:b] :c]
               [:a :b [] [:c]]
               [:a :b [] :c []]
               [:a :b [] :c]
               [:a :b [:c]]
               [:a :b :c []]
               [:a :b :c]]
       (run* [x]
             (flattenrevᵒ [[:a :b] :c] x)))

(defn reverse [l]
  (let [[new-l (list l)]]
    (.reverse new-l)
    new-l))

(frame "5.76" [[:a :b :c]
               [:a :b :c []]
               [:a :b [:c]]
               [:a :b [] :c]
               [:a :b [] :c []]
               [:a :b [] [:c]]
               [:a [:b] :c]
               [:a [:b] :c []]
               [:a [:b] [:c]]
               [[:a :b] :c]
               [[:a :b] :c []]
               [[:a :b] [:c]]
               [[[:a :b] :c]]]
       (reverse
        (run* [x]
              (flattenrevᵒ [[:a :b] :c] x))))

(frame "5.77" [(list* :a :b :c)
               [:a :b :c]]
       (run 2 [x]
            (flattenrevᵒ x [:a :b :c])))


(frame "5.80" 574
       (len (run* [x]
                  (flattenrevᵒ [[[[:a [[[:b]]] :c]]] :d] x))))
