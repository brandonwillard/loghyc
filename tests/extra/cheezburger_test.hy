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

(import [nose.tools [assert-equal assert-not-equal]]
        [adderall.dsl [*]]
        [adderall.extra.cheezburger.kitteh :as kitteh]
        [adderall.extra.cheezburger.grumpy-cat :as grumpy-cat])
(require [adderall.dsl [*]])

;; By default, using cheezburger.kitteh, you can has anything.
(defn test-cheezburger-kitteh []
  (assert-equal (run* [q] (kitteh.canhasᵒ 'cheezburger) (≡ q True))
                [True])
  (assert-equal (run* [q] (kitteh.canhasᵒ 'a-million-dollars) (≡ q True))
                [True]))

;; When using cheezburger.grumpy-cat, you can has cheezburger. You
;; want to has anything else? No.
(defn test-cheezburger-grumpy-cat []
  (assert-equal (run* [q] (grumpy-cat.canhasᵒ 'cheezburger) (≡ q True))
                [True])
  (assert-equal (run* [q] (grumpy-cat.canhasᵒ 'a-million-dollars) (≡ q True))
                []))
