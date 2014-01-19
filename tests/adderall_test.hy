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

(import [adderall.dsl [*]])
(require adderall.dsl)
(require adderall.lvar)

(defn test-fail-and-succeed []
  (assert (= (run* [q] fail) []))
  (assert (= (run* [q] succeed) [#U0])))

(defn test-#s-and-#u []
  (assert (= (run* [q] #uu) []))
  (assert (= (run* [q] #ss) [#U0])))

(defn test-fresh []
  (assert (= (run* [q] (fresh [x])) [#U0])))

(defn test-consᵒ []
  (assert (= (run* [q] (conso 1 [2 3] [1 2 3]))
             [#U0]))
  (assert (= (run* [q] (conso q [2 3] [1 2 3]))
             [1]))
  (assert (= (run* [q] (conso 1 q [1 2 3]))
             [[2 3]]))
  (assert (= (run* [q] (conso 1 [2 3] q))
             [[1 2 3]]))
  (assert (= (run* [q] (conso 1 [q 3] [1 2 3]))
             [2]))
  (assert (= (run* [q] (conso 1 [2 q] [1 2 3]))
             [3]))
  (assert (= (run* [q] (conso 1 [2 3] [q 2 3]))
             [1]))
  (assert (= (run* [q] (conso 1 [2 3] [1 q 3]))
             [2]))
  (assert (= (run* [q] (conso 1 [2 3] [1 2 q]))
             [3]))
  (assert (= (run* [q] (conso 1 2 q))
             [(cons 1 2)])))
