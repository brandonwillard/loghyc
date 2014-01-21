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

(import [adderall.dsl [*]]
        [adderall.lvar [LVar]]
        [adderall.extra.misc [*]])
(require adderall.dsl)
(require adderall.lvar)

(defn test-typeo []
  (assert (= (run* [q] (typeᵒ 2 3) (≡ q true))
             [true]))
  (assert (= (run* [q] (typeᵒ 2 "foo") (≡ q true))
             []))
  (assert (= (run* [q] (typeᵒ q 2))
             [(type 2)]))
  (assert (= (run* [q] (typeᵒ 2 q))
             [(type 2)]))
  (assert (= (run* [q] (fresh [x] (typeᵒ q x)))
             [#U0 LVar #U0])))
