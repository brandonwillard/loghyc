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

(require [adderall.dsl [*]])
(require [tests.schemer.common [*]])


(frame "3.7" [#U 0]
       (run* [x]
             (listᵒ ['a 'b x 'd])))

(frame "3.10" [[]]
       (run 1 [x]
            (listᵒ ['a 'b 'c x])))

(frame "3.14" [[]
               [#U 0]
               [#U 0 #U 1]
               [#U 0 #U 1 #U 2]
               [#U 0 #U 1 #U 2 #U 3]]
       (run 5 [x]
            (listᵒ ['a 'b 'c x])))

(frame "3.20" [[]]
       (run 1 [l]
            (lolᵒ l)))

(frame "3.21" [True]
       (run* [q]
             (fresh [x y]
                    (lolᵒ [['a 'b] [x 'c] ['d y]])
                    (≡ True q))))

(frame "3.22" [True]
       (run 1 [q]
            (fresh [x]
                   (lolᵒ (cons ['a 'b] x))
                   (≡ True q))))

(frame "3.23" [[]]
       (run 1 [x]
            (lolᵒ [['a 'b] ['c 'd] x])))

(frame "3.24" [[]
               [[]]
               [[] []]
               [[] [] []]
               [[] [] [] []]]
       (run 5 [x]
            (lolᵒ [['a 'b] ['c 'd] x])))


(frame "3.32" [True]
       (run* [q]
             (twinsᵒ ['tofu 'tofu])
             (≡ True q)))

(frame "3.33" ['tofu]
       (run* [z]
             (twinsᵒ [z 'tofu])))

(frame "3.38" [[]]
       (run 1 [z]
            (lotᵒ (cons ['g 'g] z))))

(frame "3.42" [[]
               [[#U 0 #U 0]]
               [[#U 0 #U 0] [#U 1 #U 1]]
               [[#U 0 #U 0] [#U 1 #U 1] [#U 2 #U 2]]
               [[#U 0 #U 0] [#U 1 #U 1] [#U 2 #U 2] [#U 3 #U 3]]]
       (run 5 [z]
            (lotᵒ (cons ['g 'g] z))))

(frame "3.45" [['e [#U 0 #U 0] []]
               ['e [#U 0 #U 0] [[#U 1 #U 1]]]
               ['e [#U 0 #U 0] [[#U 1 #U 1] [#U 2 #U 2]]]
               ['e [#U 0 #U 0] [[#U 1 #U 1] [#U 2 #U 2] [#U 3 #U 3]]]
               ['e [#U 0 #U 0] [[#U 1 #U 1] [#U 2 #U 2] [#U 3 #U 3] [#U 4 #U 4]]]]
       (run 5 [r]
            (fresh [w x y z]
                   (lotᵒ [['g 'g] ['e w] [x y] z])
                   (≡ [w [x y] z] r))))

(frame "3.47" [[['g 'g] ['e 'e] [#U 0 #U 0]]
               [['g 'g] ['e 'e] [#U 0 #U 0] [#U 1 #U 1]]
               [['g 'g] ['e 'e] [#U 0 #U 0] [#U 1 #U 1] [#U 2 #U 2]]]
       (run 3 [out]
            (fresh [w x y z]
                   (≡ [['g 'g] ['e w] [x y] z] out)
                   (lotᵒ out))))

(frame "3.49" [[['g 'g] ['e 'e] [#U 0 #U 0]]
               [['g 'g] ['e 'e] [#U 0 #U 0] [#U 1 #U 1]]
               [['g 'g] ['e 'e] [#U 0 #U 0] [#U 1 #U 1] [#U 2 #U 2]]]
       (run 3 [out]
            (fresh [w x y z]
                   (≡ [['g 'g] ['e w] [x y] z] out)
                   (listofᵒ twinsᵒ out))))

(frame "3.57" [True]
       (run* [q]
             (memberᵒ 'olive ['virgin 'olive 'oil])
             (≡ True q)))

(frame "3.58" ['hummus]
       (run 1 [y]
            (memberᵒ y ['hummus 'with 'pita])))

(frame "3.59" ['with]
       (run 1 [y]
            (memberᵒ y ['with 'pita])))

(frame "3.60" ['pita]
       (run 1 [y]
            (memberᵒ y ['pita])))

(frame "3.61" []
       (run* [y]
             (memberᵒ y [])))

(frame "3.62" ['hummus 'with 'pita]
       (run* [y]
             (memberᵒ y ['hummus 'with 'pita])))

(frame "3.66" ['e]
       (run* [x]
             (memberᵒ 'e ['pasta x 'fagioli])))

(frame "3.69" [#U 0]
       (run 1 [x]
            (memberᵒ 'e ['pasta 'e x 'fagioli])))

(frame "3.70" ['e]
       (run 1 [x]
            (memberᵒ 'e ['pasta x 'e 'fagioli])))

(frame "3.71" [['e #U 0] [#U 0 'e]]
       (run* [r]
             (fresh [x y]
                    (memberᵒ 'e ['pasta x 'fagioli y])
                    (≡ [x y] r))))

(frame "3.73" [(cons 'tofu #U 0)]
       (run 1 [l]
            (memberᵒ 'tofu l)))

(frame "3.76" [['tofu #U 0]
               [#U 0 'tofu #U 1]
               [#U 0 #U 1 'tofu #U 2]
               [#U 0 #U 1 #U 2 'tofu #U 3]
               [#U 0 #U 1 #U 2 #U 3 'tofu #U 4]]
       (run 5 [l]
            (memberᵒ 'tofu l)))

(frame "3.88" [True True]
       (run* [q]
             (pmemberᵒ 'tofu ['a 'b 'tofu 'd 'tofu])
             (≡ True q)))

(frame "3.94" [['tofu #U 0 #U 1]
               ['tofu]
               [#U 0 'tofu #U 1 #U 2]
               [#U 0 'tofu]
               [#U 0 #U 1 'tofu #U 2 #U 3]
               [#U 0 #U 1 'tofu]
               [#U 0 #U 1 #U 2 'tofu #U 3 #U 4]
               [#U 0 #U 1 #U 2 'tofu]
               [#U 0 #U 1 #U 2 #U 3 'tofu #U 4 #U 5]
               [#U 0 #U 1 #U 2
                #U 3 'tofu]
               [#U 0 #U 1 #U 2 #U 3 #U 4 'tofu #U 5 #U 6]
               [#U 0 #U 1 #U 2 #U 3 #U 4 'tofu]]
       (run 12 [l]
            (pmemberᵒ 'tofu l)))

(frame "3.100" ['fagioli 'e 'pasta]
       (run* [x]
             (memberrevᵒ x ['pasta 'e 'fagioli])))
