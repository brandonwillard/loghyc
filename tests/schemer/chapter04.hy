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

(import [cons [cons car cdr]])
(import [adderall.dsl [*]]
        [adderall.internal [*]]
        [tests.schemer.common [*]])

(require [adderall.dsl [*]])
(require [tests.schemer.common [*]])


(frame "4.10" [['tofu 'd 'tofu 'e]]
       (run 1 [out]
            (memᵒ 'tofu ['a 'b 'tofu 'd 'tofu 'e] out)))

(frame "4.11" [['tofu 'd 'tofu 'e]]
       (run 1 [out]
            (fresh [x]
                   (memᵒ 'tofu ['a 'b x 'd 'tofu 'e] out))))

(frame "4.12" ['tofu]
       (run* [r]
             (memᵒ r
                   ['a 'b 'tofu 'd 'tofu 'e]
                   ['tofu 'd 'tofu 'e])))

(frame "4.13" [True]
       (run* [q]
             (memᵒ 'tofu ['tofu 'e] ['tofu 'e])
             (≡ True q)))

(frame "4.14" []
       (run* [q]
             (memᵒ 'tofu ['tofu 'e] ['tofu])
             (≡ True q)))

(frame "4.15" ['tofu]
       (run* [x]
             (memᵒ 'tofu ['tofu 'e] [x 'e])))

(frame "4.16" []
       (run* [x]
             (memᵒ 'tofu ['tofu 'e] ['peas x])))

(frame "4.17" [['tofu 'd 'tofu 'e] ['tofu 'e]]
       (run* [out]
             (fresh [x]
                    (memᵒ 'tofu ['a 'b x 'd 'tofu 'e] out))))

(frame "4.18" [#U 0
               #U 0
               (cons 'tofu #U 0)
               (cons #U 0 'tofu #U 1)
               (cons #U 0 #U 1 'tofu #U 2)
               (cons #U 0 #U 1 #U 2 'tofu #U 3)
               (cons #U 0 #U 1 #U 2 #U 3 'tofu #U 4)
               (cons #U 0 #U 1 #U 2 #U 3 #U 4 'tofu #U 5)
               (cons #U 0 #U 1 #U 2 #U 3 #U 4 #U 5 'tofu #U 6)
               (cons #U 0 #U 1 #U 2 #U 3 #U 4 #U 5 #U 6 'tofu #U 7)
               (cons #U 0 #U 1 #U 2 #U 3 #U 4 #U 5 #U 6 #U 7 'tofu #U 8)
               (cons #U 0 #U 1 #U 2 #U 3 #U 4 #U 5 #U 6 #U 7 #U 8 'tofu #U 9)]
       (run 12 [z]
            (fresh [u]
                   (memᵒ 'tofu (cons 'a 'b 'tofu 'd 'tofu 'e z) u))))

(frame "4.30" [['a 'b 'd 'peas 'e]]
       (run 1 [out]
            (fresh [y]
                   (rememberᵒ 'peas ['a 'b y 'd 'peas 'e] out))))

(frame "4.31" [['b 'a 'd  #U 0 'e]
               ['a 'b 'd  #U 0 'e]
               ['a 'b 'd  #U 0 'e]
               ['a 'b 'd  #U 0 'e]
               ['a 'b #U 0 'd  'e]
               ['a 'b 'e  'd  #U 0]
               ['a 'b #U 0 'd  #U 1 'e]]
       (run* [out]
             (fresh [y z]
                    (rememberᵒ y ['a 'b y 'd z 'e] out))))

(frame "4.49" [['d  'd]
               ['d  'd]
               [#U 0 #U 0]
               ['e  'e]]
       (run* [r]
             (fresh [y z]
                    (rememberᵒ y [y 'd z 'e] [y 'd 'e])
                    (≡ [y z] r))))

(frame "4.57" [#U 0
               #U 0
               #U 0
               #U 0
               #U 0
               []
               (cons #U 0 #U 1)
               [#U 0]
               (cons #U 0 #U 1 #U 2)
               [#U 0 #U 1]
               (cons #U 0 #U 1 #U 2 #U 3)
               [#U 0 #U 1 #U 2]
               (cons #U 0 #U 1 #U 2 #U 3 #U 4)]
       (run 13 [w]
            (fresh [y z out]
                   (rememberᵒ y (cons 'a 'b y 'd z w) out))))

(defn surpriseᵒ [s]
  (rememberᵒ s ['a 'b 'c] ['a 'b 'c]))

(frame "4.69" ['d]
       (run* [r]
             (≡ 'd r)
             (surpriseᵒ r)))

(frame "4.70" [#U 0]
       (run* [r]
             (surpriseᵒ r)))

(frame "4.72" ['b]
       (run* [r]
             (≡ 'b r)
             (surpriseᵒ r)))
