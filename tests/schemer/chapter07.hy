;; adderall - miniKanren in Hy
;; Copyright (C) 2014  Gergely Nagy <algernon@madhouse-project.org>
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
        [adderall.bitnum [*]]
        [adderall.internal [*]])
(import [adderall.bitnum [-ᵒ -o]])
(import [tests.schemer.common [*]])

(require [adderall.dsl [*]])
(require [tests.schemer.common [*]])


(frame "7.6" [[0 0] [1 1]]
       (run* [s]
             (fresh [x y]
                    (xorᵒ x y 0)
                    (≡ [x y] s))))

(frame "7.8" [[1 0] [0 1]]
       (run* [s]
             (fresh [x y]
                    (xorᵒ x y 1)
                    (≡ [x y] s))))

(frame "7.9" [[0 0 0]
              [1 0 1]
              [0 1 1]
              [1 1 0]]
       (run* [s]
             (fresh [x y r]
                    (xorᵒ x y r)
                    (≡ [x y r] s))))

(frame "7.11" [[1 1]]
       (run* [s]
             (fresh [x y]
                    (andᵒ x y 1)
                    (≡ [x y] s))))

(frame "7.12" [0]
       (run* [r]
             (half-adderᵒ 1 1 r 1)))

(frame "7.13" [[0 0 0 0]
               [1 0 1 0]
               [0 1 1 0]
               [1 1 0 1]]
       (run* [s]
             (fresh [x y r c]
                    (half-adderᵒ x y r c)
                    (≡ [x y r c] s))))

(frame "7.15" [[0 1]]
       (run* [s]
             (fresh [r c]
                    (full-adderᵒ 0 1 1 r c)
                    (≡ [r c] s))))

(frame "7.16" [[1 1]]
       (run* [s]
             (fresh [r c]
                    (full-adderᵒ 1 1 1 r c)
                    (≡ [r c] s))))

(frame "7.17" [[0 0 0 0 0]
               [1 0 0 1 0]
               [0 1 0 1 0]
               [1 1 0 0 1]
               [0 0 1 1 0]
               [1 0 1 0 1]
               [0 1 1 0 1]
               [1 1 1 1 1]]
       (run* [s]
             (fresh [b x y r c]
                    (full-adderᵒ b x y r c)
                    (≡ [b x y r c] s))))

(frame "7.80" [True]
       (run* [q]
             (posᵒ [0 1 1])
             (≡ True q)))

(frame "7.81" [True]
       (run* [q]
             (posᵒ [1])
             (≡ True q)))

(frame "7.82" []
       (run* [q]
             (posᵒ [])
             (≡ True q)))

(frame "7.83" [(cons #U 0 #U 1)]
       (run* [r]
             (posᵒ r)))

(frame "7.86" [True]
       (run* [q]
             (>1ᵒ [0 1 1])
             (≡ q True)))

(frame "7.87" [True]
       (run* [q]
             (>1ᵒ [0 1])
             (≡ q True)))

(frame "7.88" []
       (run* [q]
             (>1ᵒ [1])
             (≡ q True)))

(frame "7.89" []
       (run* [q]
             (>1ᵒ [])
             (≡ q True)))

(frame "7.90" [(cons #U 0 #U 1 #U 2)]
       (run* [r]
             (>1ᵒ r)))

(frame "7.97" [[#U 0 [] #U 0]
               [[] (cons #U 0 #U 1) (cons #U 0 #U 1)]
               [[1] [1] [0 1]]]
       (run 3 [s]
            (fresh [x y r]
                   (adderᵒ 0 x y r)
                   (≡ [x y r] s))))

;; This is disabled, because the order of solutions is different,
;; likely because adderall.bitnum's adderᵒ does not use allⁱ (yet).
#_(frame "7.101" [[#U 0 (,) #U 0]
                  [[] (cons #U 0 #U 1) (cons #U 0 #U 1)]
                  [[1] [1] [0 1]]
                  [[1] [0 #U 0 #U 1] [1 #U 0 #U 1]]
                  [[0 #U 0 #U 1] [1] [1 #U 0 #U 1]]
                  [[0 1] [0 1] [0 0 1]]
                  [[1] [1 1] [0 0 1]]
                  [[1] [1 0 #U 0 #U 1] [0 1 #U 0 #U 1]]
                  [[1 1] [1] [0 0 1]]
                  [[1] [1 1 1] [0 0 0 1]]
                  [[1 1] [0 1] [1 0 1]]
                  [[1] [1 1 0 #U 0 #U 1] [0 0 1 #U 0 #U 1]]
                  [[1 0 #U 0 #U 1] [1] [0 1 #U 0 #U 1]]
                  [[1] [1 1 1 1] [0 0 0 0 1]]
                  [[0 1] [0 0 #U 0 #U 1] [0 1 #U 0 #U 1]]
                  [[1] [1 1 1 0 #U 0 #U 1] [0 0 0 1 #U 0 #U 1]]
                  [[1 1 1] [1] [0 0 0 1]]
                  [[1] [1 1 1 1 1] [0 0 0 0 0 1]]
                  [[0 1] [1 1] [1 0 1]]
                  [[1] [1 1 1 1 0 #U 0 #U 1] [0 0 0 0 1 #U 0 #U 1]]
                  [[1 1 0 #U 0 #U 1] [1] [0 0 1 #U 1 #U 1]]
                  [[1] [1 1 1 1 1 1] [0 0 0 0 0 0 1]]]
         (run 22 [s]
              (fresh [x y r]
                     (adderᵒ 0 x y r)
                     (≡ [x y r] s))))

(frame "7.120" [[0 1 0 1]]
       (run* [s]
             (gen-adderᵒ 1 [0 1 1] [1 1] s)))

(frame "7.126" [[[1 0 1] []]
                [[] [1 0 1]]
                [[1] [0 0 1]]
                [[0 0 1] [1]]
                [[1 1] [0 1]]
                [[0 1] [1 1]]]
       (run* [s]
             (fresh [x y]
                    (adderᵒ 0 x y [1 0 1])
                    (≡ [x y] s))))

(frame "7.129" [[[1 0 1] []]
                [[] [1 0 1]]
                [[1] [0 0 1]]
                [[0 0 1] [1]]
                [[1 1] [0 1]]
                [[0 1] [1 1]]]
       (run* [s]
             (fresh [x y]
                    (+ᵒ x y [1 0 1])
                    (≡ [x y] s))))

(frame "7.131" [[1 1]]
       (run* [q]
             (-ᵒ [0 0 0 1] [1 0 1] q)))

(frame "7.132" [[]]
       (run* [q]
             (-ᵒ [0 1 1] [0 1 1] q)))

(frame "7.133" []
       (run* [q]
             (-ᵒ [0 1 1] [0 0 0 1] q)))
