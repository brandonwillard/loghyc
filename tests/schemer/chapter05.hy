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

(import [adderall.dsl [*]]
        [adderall.internal [*]]
        [tests.schemer.common [*]])

(require [hy.contrib.walk [let]])
(require [adderall.dsl [*]])
(require [tests.schemer.common [*]])


(frame "5.10" [['cake 'tastes 'yummy]]
       (run* [x]
             (appendᵒ ['cake]
                      ['tastes 'yummy]
                      x)))

(frame "5.11" [['cake 'with 'ice #U 0 'tastes 'yummy]]
       (run* [x]
             (fresh [y]
                    (appendᵒ ['cake 'with 'ice y]
                             ['tastes 'yummy]
                             x))))

(frame "5.12" [(cons 'cake 'with 'ice 'cream #U 0)]
       (run* [x]
             (fresh [y]
                    (appendᵒ ['cake 'with 'ice 'cream]
                             y
                             x))))

(frame "5.13" [['cake 'with 'ice 'd 't]]
       (run 1 [x]
            (fresh [y]
                   (appendᵒ (cons 'cake 'with 'ice y)
                            ['d 't]
                            x))))

(frame "5.14" [[]]
       (run 1 [y]
            (fresh [x]
                   (appendᵒ (cons 'cake 'with 'ice y)
                            ['d 't]
                            x))))


(frame "5.16" [['cake 'with 'ice 'd 't]
               ['cake 'with 'ice #U 0 'd 't]
               ['cake 'with 'ice #U 0 #U 1 'd 't]
               ['cake 'with 'ice #U 0 #U 1 #U 2 'd 't]
               ['cake 'with 'ice #U 0 #U 1 #U 2 #U 3 'd 't]]
       (run 5 [x]
            (fresh [y]
                   (appendᵒ (cons 'cake 'with 'ice y)
                            ['d 't]
                            x))))

(frame "5.17" [[]
               [#U 0]
               [#U 0 #U 1]
               [#U 0 #U 1 #U 2]
               [#U 0 #U 1 #U 2 #U 3]]
       (run 5 [y]
            (fresh [x]
                   (appendᵒ (cons 'cake 'with 'ice y)
                            ['d 't]
                            x))))

(frame "5.20" [['cake 'with 'ice 'd 't]
               ['cake 'with 'ice #U 0 'd 't #U 0]
               ['cake 'with 'ice #U 0 #U 1 'd 't #U 0 #U 1]
               ['cake 'with 'ice #U 0 #U 1 #U 2 'd 't #U 0 #U 1 #U 2]
               ['cake 'with 'ice #U 0 #U 1 #U 2 #U 3 'd 't #U 0 #U 1 #U 2 #U 3]]
       (run 5 [x]
            (fresh [y]
                   (appendᵒ (cons 'cake 'with 'ice y)
                            (cons 'd 't y)
                            x))))

(frame "5.21" [['cake 'with 'ice 'cream 'd 't #U 0]]
       (run* [x]
             (fresh [z]
                    (appendᵒ ['cake 'with 'ice 'cream]
                             ['d 't z]
                             x))))

(frame "5.23" [[]
               ['cake]
               ['cake 'with]
               ['cake 'with 'ice]
               ['cake 'with 'ice 'd]
               ['cake 'with 'ice 'd 't]]
       (run 6 [x]
            (fresh [y]
                   (appendᵒ x y ['cake 'with 'ice 'd 't]))))

(frame "5.25" [['cake 'with 'ice 'd 't]
               ['with 'ice 'd 't]
               ['ice 'd 't]
               ['d 't]
               ['t]
               []]
       (run 6 [y]
            (fresh [x]
                   (appendᵒ x y ['cake 'with 'ice 'd 't]))))

(frame "5.27" [[[] ['cake 'with 'ice 'd 't]]
               [['cake] ['with 'ice 'd 't]]
               [['cake 'with] ['ice 'd 't]]
               [['cake 'with 'ice] ['d 't]]
               [['cake 'with 'ice 'd] ['t]]
               [['cake 'with 'ice 'd 't] []]]
       (run 6 [r]
            (fresh [x y]
                   (appendᵒ x y ['cake 'with 'ice 'd 't])
                   (≡ [x y] r))))

(frame "5.32" [[[] ['cake 'with 'ice 'd 't]]
               [['cake] ['with 'ice 'd 't]]
               [['cake 'with] ['ice 'd 't]]
               [['cake 'with 'ice] ['d 't]]
               [['cake 'with 'ice 'd] ['t]]
               [['cake 'with 'ice 'd 't] []]]
       (run 7 [r]
            (fresh [x y]
                   (appendᵒ x y ['cake 'with 'ice 'd 't])
                   (≡ [x y] r))))

(frame "5.33" [[]
               [#U 0]
               [#U 0 #U 1]
               [#U 0 #U 1 #U 2]
               [#U 0 #U 1 #U 2 #U 3]
               [#U 0 #U 1 #U 2 #U 3 #U 4]
               [#U 0 #U 1 #U 2 #U 3 #U 4 #U 5]]
       (run 7 [x]
            (fresh [y z]
                   (appendᵒ x y z))))

(frame "5.34" [#U 0 #U 0 #U 0 #U 0 #U 0 #U 0 #U 0]
       (run 7 [y]
            (fresh [x z]
                   (appendᵒ x y z))))

(frame "5.36" [#U 0
               (cons #U 0 #U 1)
               (cons #U 0 #U 1 #U 2)
               (cons #U 0 #U 1 #U 2 #U 3)
               (cons #U 0 #U 1 #U 2 #U 3 #U 4)
               (cons #U 0 #U 1 #U 2 #U 3 #U 4 #U 5)
               (cons #U 0 #U 1 #U 2 #U 3 #U 4 #U 5 #U 6)]
       (run 7 [z]
            (fresh [x y]
                   (appendᵒ x y z))))

(frame "5.37" [[[] #U 0 #U 0]
               [[#U 0] #U 1 (cons #U 0 #U 1)]
               [[#U 0 #U 1] #U 2 (cons #U 0 #U 1 #U 2)]
               [[#U 0 #U 1 #U 2] #U 3 (cons #U 0 #U 1 #U 2 #U 3)]
               [[#U 0 #U 1 #U 2 #U 3] #U 4 (cons #U 0 #U 1 #U 2 #U 3 #U 4)]
               [[#U 0 #U 1 #U 2 #U 3 #U 4] #U 5 (cons #U 0 #U 1 #U 2 #U 3 #U 4 #U 5)]
               [[#U 0 #U 1 #U 2 #U 3 #U 4 #U 5] #U 6 (cons #U 0 #U 1 #U 2 #U 3 #U 4 #U 5 #U 6)]]
       (run 7 [r]
            (fresh [x y z]
                   (appendᵒ x y z)
                   (≡ [x y z] r))))

;; The order is different here than in The Reasoned Schemer, because
;; in frame 5.52, we swap the conditions, which reverses the order.
(frame "5.46" [[[['pizza]]]
               [['pizza]]
               ['pizza]
               'pizza]
       (run* [x]
             (unwrapᵒ [[['pizza]]] x)))

(frame "5.53" ['pizza
               (cons 'pizza #U 0)
               (cons (cons 'pizza #U 0) #U 1)
               (cons (cons (cons 'pizza #U 0) #U 1) #U 2)
               (cons (cons (cons (cons 'pizza #U 0) #U 1) #U 2) #U 3)]
       (run 5 [x]
            (unwrapᵒ x 'pizza)))

(frame "5.54" [[['pizza]]
               (cons [['pizza]] #U 0)
               (cons (cons [['pizza]] #U 0) #U 1)
               (cons (cons (cons [['pizza]] #U 0) #U 1) #U 2)
               (cons (cons (cons (cons [['pizza]] #U 0) #U 1) #U 2) #U 3)]
       (run 5 [x]
            (unwrapᵒ x [['pizza]])))

(frame "5.55" ['pizza
               (cons 'pizza #U 0)
               (cons (cons 'pizza #U 0) #U 1)
               (cons (cons (cons 'pizza #U 0) #U 1) #U 2)
               (cons (cons (cons (cons 'pizza #U 0) #U 1) #U 2) #U 3)]
       (run 5 [x]
            (unwrapᵒ [[x]] 'pizza)))

(frame "5.60" [['a 'b 'c]]
       (run 1 [x]
            (flattenᵒ [['a 'b] 'c] x)))

(frame "5.61" [['a 'b 'c]]
       (run 1 [x]
            (flattenᵒ ['a ['b 'c]] x)))

(frame "5.62" [['a]
               ['a []]
               [['a]]]
       (run* [x]
             (flattenᵒ ['a] x)))

(frame "5.64" [['a]
               ['a []]
               ['a []]
               ['a [] []]
               [['a]]
               [['a] []]
               [[['a]]]]
       (run* [x]
             (flattenᵒ [['a]] x)))

(frame "5.66" [['a]
               ['a []]
               ['a []]
               ['a [] []]
               ['a []]
               ['a [] []]
               ['a [] []]
               ['a [] [] []]
               [['a]]
               [['a] []]
               [['a] []]
               [['a] [] []]
               [[['a]]]
               [[['a]] []]
               [[[['a]]]]]
       (run* [x]
             (flattenᵒ [[['a]]] x)))

(frame "5.68" [['a 'b 'c]
               ['a 'b 'c []]
               ['a 'b ['c]]
               ['a 'b [] 'c]
               ['a 'b [] 'c []]
               ['a 'b [] ['c]]
               ['a ['b] 'c]
               ['a ['b] 'c []]
               ['a ['b] ['c]]
               [['a 'b] 'c]
               [['a 'b] 'c []]
               [['a 'b] ['c]]
               [[['a 'b] 'c]]]
       (run* [x]
             (flattenᵒ [['a 'b] 'c] x)))

(frame "5.75" [[[['a 'b] 'c]]
               [['a 'b] ['c]]
               [['a 'b] 'c []]
               [['a 'b] 'c]
               ['a ['b] ['c]]
               ['a ['b] 'c []]
               ['a ['b] 'c]
               ['a 'b [] ['c]]
               ['a 'b [] 'c []]
               ['a 'b [] 'c]
               ['a 'b ['c]]
               ['a 'b 'c []]
               ['a 'b 'c]]
       (run* [x]
             (flattenrevᵒ [['a 'b] 'c] x)))

(defn reverse [l]
  (let [new-l (list l)]
    (.reverse new-l)
    new-l))

(frame "5.76" [['a 'b 'c]
               ['a 'b 'c []]
               ['a 'b ['c]]
               ['a 'b [] 'c]
               ['a 'b [] 'c []]
               ['a 'b [] ['c]]
               ['a ['b] 'c]
               ['a ['b] 'c []]
               ['a ['b] ['c]]
               [['a 'b] 'c]
               [['a 'b] 'c []]
               [['a 'b] ['c]]
               [[['a 'b] 'c]]]
       (reverse
        (run* [x]
              (flattenrevᵒ [['a 'b] 'c] x))))

(frame "5.77" [(cons 'a 'b 'c)
               ['a 'b 'c]]
       (run 2 [x]
            (flattenrevᵒ x ['a 'b 'c])))


(frame "5.80" 574
       (len (run* [x]
                  (flattenrevᵒ [[[['a [[['b]]] 'c]]] 'd] x))))
