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

(frame "3.7" [#U0]
       (run* [x]
             (listᵒ [:a :b x :d])))

(frame "3.10" [[]]
       (run 1 [x]
            (listᵒ (list* :a :b :c x))))

(frame "3.14" [[]
               [#U0]
               [#U0 #U1]
               [#U0 #U1 #U2]
               [#U0 #U1 #U2 #U3]]
       (run 5 [x]
            (listᵒ (list* :a :b :c x))))

(frame "3.20" [[]]
       (run 1 [l]
            (lolᵒ l)))

(frame "3.21" [true]
       (run* [q]
             (fresh [x y]
                    (lolᵒ [[:a :b] [x :c] [:d y]])
                    (≡ true q))))

(frame "3.22" [true]
       (run 1 [q]
            (fresh [x]
                   (lolᵒ (cons [:a :b] x))
                   (≡ true q))))

(frame "3.23" [[]]
       (run 1 [x]
            (lolᵒ (list* [:a :b] [:c :d] x))))

(frame "3.24" [[]
               [[]]
               [[] []]
               [[] [] []]
               [[] [] [] []]]
       (run 5 [x]
            (lolᵒ (list* [:a :b] [:c :d] x))))


(frame "3.32" [true]
       (run* [q]
             (twinsᵒ [:tofu :tofu])
             (≡ true q)))

(frame "3.33" [:tofu]
       (run* [z]
             (twinsᵒ [z :tofu])))

(frame "3.38" [[]]
         (run 1 [z]
              (lotᵒ (cons [:g :g] z))))

(frame "3.42" [[]
               [[#U0 #U0]]
               [[#U0 #U0] [#U1 #U1]]
               [[#U0 #U0] [#U1 #U1] [#U2 #U2]]
               [[#U0 #U0] [#U1 #U1] [#U2 #U2] [#U3 #U3]]]
       (run 5 [z]
            (lotᵒ (cons [:g :g] z))))

(frame "3.45" [[:e [#U0 #U0] []]
               [:e [#U0 #U0] [[#U1 #U1]]]
               [:e [#U0 #U0] [[#U1 #U1] [#U2 #U2]]]
               [:e [#U0 #U0] [[#U1 #U1] [#U2 #U2] [#U3 #U3]]]
               [:e [#U0 #U0] [[#U1 #U1] [#U2 #U2] [#U3 #U3] [#U4 #U4]]]]
       (run 5 [r]
            (fresh [w x y z]
                   (lotᵒ (list* [:g :g] [:e w] [x y] z))
                   (≡ [w [x y] z] r))))

(frame "3.47" [[[:g :g] [:e :e] [#U0 #U0]]
                 [[:g :g] [:e :e] [#U0 #U0] [#U1 #U1]]
                 [[:g :g] [:e :e] [#U0 #U0] [#U1 #U1] [#U2 #U2]]]
         (run 3 [out]
              (fresh [w x y z]
                     (≡ (list* [:g :g] [:e w] [x y] z) out)
                     (lotᵒ out))))

(frame "3.49" [[[:g :g] [:e :e] [#U0 #U0]]
               [[:g :g] [:e :e] [#U0 #U0] [#U1 #U1]]
               [[:g :g] [:e :e] [#U0 #U0] [#U1 #U1] [#U2 #U2]]]
       (run 3 [out]
            (fresh [w x y z]
                   (≡ (list* [:g :g] [:e w] [x y] z) out)
                   (listofᵒ twinsᵒ out))))

(frame "3.57" [true]
       (run* [q]
             (memberᵒ :olive [:virgin :olive :oil])
             (≡ true q)))

(frame "3.58" [:hummus]
       (run 1 [y]
            (memberᵒ y [:hummus :with :pita])))

(frame "3.59" [:with]
         (run 1 [y]
              (memberᵒ y [:with :pita])))

(frame "3.60" [:pita]
       (run 1 [y]
            (memberᵒ y [:pita])))

(frame "3.61" []
       (run* [y]
             (memberᵒ y [])))

(frame "3.62" [:hummus :with :pita]
       (run* [y]
             (memberᵒ y [:hummus :with :pita])))

(frame "3.66" [:e]
       (run* [x]
             (memberᵒ :e [:pasta x :fagioli])))

(frame "3.69" [#U0]
       (run 1 [x]
            (memberᵒ :e [:pasta :e x :fagioli])))

(frame "3.70" [:e]
       (run 1 [x]
            (memberᵒ :e [:pasta x :e :fagioli])))

(frame "3.71" [[:e #U0] [#U0 :e]]
       (run* [r]
             (fresh [x y]
                    (memberᵒ :e [:pasta x :fagioli y])
                    (≡ [x y] r))))

(frame "3.73" [(cons :tofu #U0)]
       (run 1 [l]
            (memberᵒ :tofu l)))

(frame "3.76" [(list* :tofu #U0)
               (list* #U0 :tofu #U1)
               (list* #U0 #U1 :tofu #U2)
               (list* #U0 #U1 #U2 :tofu #U3)
               (list* #U0 #U1 #U2 #U3 :tofu #U4)]
       (run 5 [l]
            (memberᵒ :tofu l)))

(frame "3.88" [true true]
       (run* [q]
             (pmemberᵒ :tofu [:a :b :tofu :d :tofu])
             (≡ true q)))

(frame "3.94" [(list* :tofu #U0 #U1)
               [:tofu]
               (list* #U0 :tofu #U1 #U2)
               [#U0 :tofu]
               (list* #U0 #U1 :tofu #U2 #U3)
               [#U0 #U1 :tofu]
               (list* #U0 #U1 #U2 :tofu #U3 #U4)
               [#U0 #U1 #U2 :tofu]
               (list* #U0 #U1 #U2 #U3 :tofu #U4 #U5)
               [#U0 #U1 #U2
                #U3 :tofu]
               (list* #U0 #U1 #U2 #U3 #U4 :tofu #U5 #U6)
               [#U0 #U1 #U2 #U3 #U4 :tofu]]
       (run 12 [l]
            (pmemberᵒ :tofu l)))

(frame "3.100" [:fagioli :e :pasta]
       (run* [x]
             (memberrevᵒ x [:pasta :e :fagioli])))
