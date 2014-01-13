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
        [adderall.contrib.cheezburger.kitteh :as kitteh]
        [adderall.contrib.cheezburger.grumpy-cat :as grumpy-cat])
(require adderall.dsl)

;; By default, using cheezburger.kitteh, you can has anything.
(defn test-cheezburger-kitteh []
  (assert (= (run* [q] (kitteh.canhasᵒ :cheezburger) (≡ q true))
             [true]))
  (assert (= (run* [q] (kitteh.canhasᵒ 'a-million-dollars) (≡ q true))
             [true])))

;; When using cheezburger.grumpy-cat, you can has cheezburger. You
;; want to has anything else? No.
(defn test-cheezburger-grumpy-cat []
  (assert (= (run* [q] (grumpy-cat.canhasᵒ :cheezburger) (≡ q true))
             [true]))
  (assert (= (run* [q] (grumpy-cat.canhasᵒ 'a-million-dollars) (≡ q true))
             [])))
