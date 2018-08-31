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
        [tests.schemer.common [*]])
(require [adderall.dsl [*]])
(require [tests.schemer.common [*]])

(frame "6.6" []
       (run 1 [q]
            u#
            neverᵒ))

(frame "6.7" [True]
       (run 1 [q]
            alwaysᵒ
            (≡ True q)))

(frame "6.10" [True True True True True]
       (run 5 [q]
            alwaysᵒ
            (≡ True q)))

(frame "6.11" [True True True True True]
       (run 5 [q]
            (≡ True q)
            alwaysᵒ))

(frame "6.13" [True]
       (run 1 [q]
            (salᵒ alwaysᵒ)
            (≡ True q)))

(frame "6.14" [True]
       (run 1 [q]
            (salᵒ neverᵒ)
            (≡ True q)))

(frame "6.19" [True]
       (run 1 [q]
            (condⁱ
             [(≡ False q) alwaysᵒ]
             [(≡ True q)])
            (≡ True q)))

(frame "6.21" [True True True True True]
       (run 5 [q]
            (condⁱ
             [(≡ False q) alwaysᵒ]
             [(anyᵒ (≡ True q))])
            (≡ True q)))

(defn teacupᵒ [x]
  (condᵉ
   [(≡ 'tea x) s#]
   [(≡ 'cup x) s#]
   (else u#)))

(frame "6.24" ['tea False 'cup]
       (run 5 [r]
            (condⁱ
             [(teacupᵒ r) s#]
             [(≡ False r) s#]
             [s# u#])))

(frame "6.25" [True True True True True]
       (run 5 [q]
            (condⁱ
             [(≡ False q) alwaysᵒ]
             [(≡ True q) alwaysᵒ]
             [s# u#])
            (≡ True q)))

(frame "6.28" [True True True True True]
       (run 5 [q]
            (condᵉ
             [alwaysᵒ s#]
             (else neverᵒ))
            (≡ True q)))

(frame "6.32" [True]
       (run 1 [q]
            (allⁱ
             (condᵉ
              [(≡ False q) s#]
              (else (≡ True q)))
             alwaysᵒ)
            (≡ True q)))

(frame "6.33" [True True True True True]
       (run 5 [q]
            (allⁱ
             (condᵉ
              [(≡ False q) s#]
              (else (≡ True q)))
             alwaysᵒ)
            (≡ True q)))

(frame "6.34" [True True True True True]
       (run 5 [q]
            (allⁱ
             (condᵉ
              [(≡ True q) s#]
              (else (≡ False q)))
             alwaysᵒ)
            (≡ True q)))

(frame "6.36" [True True True True True]
       (run 5 [q]
            (all
             (condᵉ
              [(≡ True q) s#]
              (else (≡ False q)))
             alwaysᵒ)
            (≡ True q)))
