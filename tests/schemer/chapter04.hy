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

#_(frame "4.18" [(unbound 0)
                 (unbound 0)
                 (cons [(unbound 0) :tofu] (unbound 1))
                 (cons [(unbound 0) (unbound 1) :tofu] (unbound 2))
                 (cons [(unbound 0) (unbound 1) (unbound 2) :tofu]
                       (unbound 3))
                 (cons [(unbound 0) (unbound 1) (unbound 2) (unbound 3) :tofu]
                       (unbound 4))
                 (cons [(unbound 0) (unbound 1) (unbound 2) (unbound 3)
                        (unbound 4) :tofu]
                       (unbound 5))
                 (cons [(unbound 0) (unbound 1) (unbound 2) (unbound 3)
                        (unbound 4) (unbound 5) :tofu]
                       (unbound 6))
                 (cons [(unbound 0) (unbound 1) (unbound 2) (unbound 3)
                        (unbound 4) (unbound 5) (unbound 6) :tofu]
                       (unbound 7))
                 (cons [(unbound 0) (unbound 1) (unbound 2) (unbound 3)
                        (unbound 4) (unbound 5) (unbound 6) (unbound 7)
                        :tofu]
                       (unbound 8))
                 (cons [(unbound 0) (unbound 1) (unbound 2) (unbound 3)
                        (unbound 4) (unbound 5) (unbound 6) (unbound 7)
                        (unbound 8) :tofu]
                       (unbound 9))]
         (run 12 [z]
              (fresh [u]
                     (memᵒ :tofu (cons [:a :b :tofu :d :tofu :e] z) u))))

(frame "4.30" [[:a :b :d :peas :e]]
       (run 1 [out]
            (fresh [y]
                   (rememberᵒ :peas [:a :b y :d :peas :e] out))))

(frame "4.31" [[:b :a :d (unbound 0) :e]
               [:a :b :d (unbound 0) :e]
               [:a :b :d (unbound 0) :e]
               [:a :b :d (unbound 0) :e]
               [:a :b (unbound 0) :d :e]
               [:a :b :e :d (unbound 0)]
               [:a :b (unbound 0) :d (unbound 1) :e]]
       (run* [out]
             (fresh [y z]
                    (rememberᵒ y [:a :b y :d z :e] out))))

;; FIXME: This fails, becuase the last one ends up being
;;        [:e (unbound 0)].
#_(frame "4.49" [[:d :d]
                 [:d :d]
                 [(unbound 0) (unbound 0)]
                 [:e :e]]
         (run* [r]
               (fresh [y z]
                      (rememberᵒ y [y :d z :e] [y :d :e])
                      (≡ [y z] r))))

;; FIXME: This only returns the first 5 values, none of the others.
#_(frame "4.57" [(unbound 0)
                 (unbound 0)
                 (unbound 0)
                 (unbound 0)
                 (unbound 0)
                 []
                 (cons (unbound 0) (unbound 1))
                 [(unbound 0)]
                 (list* (unbound 0) (unbound 1) (unbound 2))
                 [(unbound 0) (unbound 1)]
                 (list* (unbound 0) (unbound 1) (unbound 2) (unbound 3))
                 [(unbound 0) (unbound 1) (unbound 3)]
                 (list* (unbound 0) (unbound 1) (unbound 2) (unbound 3)
                        (unbound 4))]
         (run 13 [w]
              (fresh [y z out]
                     (rememberᵒ y (list* :a :b y :d z :w) out))))

(defn surpriseᵒ [s]
  (rememberᵒ s [:a :b :c] [:a :b :c]))

(frame "4.69" [:d]
       (run* [r]
             (≡ :d r)
             (surpriseᵒ r)))

(frame "4.70" [(unbound 0)]
       (run* [r]
             (surpriseᵒ r)))

(frame "4.72" [:b]
       (run* [r]
             (≡ :b r)
             (surpriseᵒ r)))
