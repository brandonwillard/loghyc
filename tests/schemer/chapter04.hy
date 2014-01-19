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

(frame "4.10" [[:tofu :d :tofu :e]]
       (run 1 [out]
            (memᵒ :tofu [:a :b :tofu :d :tofu :e] out)))

(frame "4.11" [[:tofu :d :tofu :e]]
       (run 1 [out]
            (fresh [x]
                   (memᵒ :tofu [:a :b x :d :tofu :e] out))))

(frame "4.12" [:tofu]
       (run* [r]
             (memᵒ r
                   [:a :b :tofu :d :tofu :e]
                   [:tofu :d :tofu :e])))

(frame "4.13" [true]
       (run* [q]
             (memᵒ :tofu [:tofu :e] [:tofu :e])
             (≡ true q)))

(frame "4.14" []
       (run* [q]
             (memᵒ :tofu [:tofu :e] [:tofu])
             (≡ true q)))

(frame "4.15" [:tofu]
       (run* [x]
             (memᵒ :tofu [:tofu :e] [x :e])))

(frame "4.16" []
       (run* [x]
             (memᵒ :tofu [:tofu :e] [:peas x])))

(frame "4.17" [[:tofu :d :tofu :e] [:tofu :e]]
       (run* [out]
             (fresh [x]
                    (memᵒ :tofu [:a :b x :d :tofu :e] out))))

#_(frame "4.18" [#U0
                 #U0
                 (cons [#U0 :tofu] #U1)
                 (cons [#U0 #U1 :tofu] #U2)
                 (cons [#U0 #U1 #U2 :tofu] #U3)
                 (cons [#U0 #U1 #U2 #U3 :tofu] #U4)
                 (cons [#U0 #U1 #U2 #U3 #U4 :tofu] #U5)
                 (cons [#U0 #U1 #U2 #U3 #U4 #U5 :tofu] #U6)
                 (cons [#U0 #U1 #U2 #U3 #U4 #U5 #U6 :tofu] #U7)
                 (cons [#U0 #U1 #U2 #U3 #U4 #U5 #U6 #U7 :tofu] #U8)
                 (cons [#U0 #U1 #U2 #U3 #U4 #U5 #U6 #U7 #U8 :tofu] #U9)]
         (run 12 [z]
              (fresh [u]
                     (memᵒ :tofu (cons [:a :b :tofu :d :tofu :e] z) u))))

(frame "4.30" [[:a :b :d :peas :e]]
       (run 1 [out]
            (fresh [y]
                   (rememberᵒ :peas [:a :b y :d :peas :e] out))))

(frame "4.31" [[:b :a :d  #U0 :e]
               [:a :b :d  #U0 :e]
               [:a :b :d  #U0 :e]
               [:a :b :d  #U0 :e]
               [:a :b #U0 :d  :e]
               [:a :b :e  :d  #U0]
               [:a :b #U0 :d  #U1 :e]]
       (run* [out]
             (fresh [y z]
                    (rememberᵒ y [:a :b y :d z :e] out))))

(frame "4.49" [[:d  :d]
               [:d  :d]
               [#U0 #U0]
               [:e  :e]]
       (run* [r]
             (fresh [y z]
                    (rememberᵒ y [y :d z :e] [y :d :e])
                    (≡ [y z] r))))

;; FIXME: This only returns the first 5 values, none of the others.
#_(frame "4.57" [#U0
                 #U0
                 #U0
                 #U0
                 #U0
                 []
                 (cons #U0 #U1)
                 [#U0]
                 (list* #U0 #U1 #U2)
                 [#U0 #U1]
                 (list* #U0 #U1 #U2 #U3)
                 [#U0 #U1 #U3]
                 (list* #U0 #U1 #U2 #U3 #U4)]
         (run 13 [w]
              (fresh [y z out]
                     (rememberᵒ y (list* :a :b y :d z :w) out))))

(defn surpriseᵒ [s]
  (rememberᵒ s [:a :b :c] [:a :b :c]))

(frame "4.69" [:d]
       (run* [r]
             (≡ :d r)
             (surpriseᵒ r)))

(frame "4.70" [#U0]
       (run* [r]
             (surpriseᵒ r)))

(frame "4.72" [:b]
       (run* [r]
             (≡ :b r)
             (surpriseᵒ r)))
