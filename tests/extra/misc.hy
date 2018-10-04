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

(import [nose.tools [assert-equal assert-not-equal]]
        [adderall.dsl [*]]
        [adderall.internal [LVar]]
        [adderall.extra.misc [*]])

(require [adderall.dsl [*]])


(defn test-typeo []
  (assert-equal (run* [q] (typeᵒ 2 3) (≡ q True))
                [True])
  (assert-equal (run* [q] (typeᵒ 2 "foo") (≡ q True))
                [])
  (assert-equal (run* [q] (typeᵒ q 2))
                [(type 2)])
  (assert-equal (run* [q] (typeᵒ 2 q))
                [(type 2)])
  (assert-equal (run* [q] (fresh [x] (typeᵒ q x)))
                [#U 0])
  (assert-equal (run* [q] (fresh [x] (≡ x 2) (typeᵒ x q)))
                [(type 2)])
  (assert-equal (run* [q] (fresh [x] (≡ x 2) (typeᵒ q x)))
                [(type 2)]))
