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
        [adderall.extra.zebra [*]])
(require adderall.dsl)
(require adderall.extra.zebra)

(defn test-zebrap []
  (assert (= (run* [q] (zebra-tableᵖ q))
             [[['norwegian  'kools         (unbound 0) 'fox        'yellow]
               ['ukrainian  'chesterfields 'tea        'horse      'blue]
               ['englishman 'oldgods       'milk       'snails     'red]
               ['spaniard   'lucky-strikes 'oj         'dog        'ivory]
               ['japanese   'parliaments   'coffee     (unbound 1) 'green]]]))
  (assert (= (run* [water horse] (zebraᵖ water horse))
             [['norwegian 'japanese]])))
