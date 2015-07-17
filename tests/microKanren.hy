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

(import [adderall.microKanren [*]]
        [hy [HySymbol]])

(def empty-state (cons '() 0))

(defmacro test-check [name actual expected]
  `(defn ~(HySymbol (+ "test-" name)) []
     (assert (= ~actual ~expected))))


(def a-and-b
  (conj
   (call/fresh (lambda [a] (== a 7)))
   (call/fresh
    (lambda [b]
      (disj
       (== b 5)
       (== b 6))))))

(test-check second-set-t1
            (let [[$ ((call/fresh (lambda [q] (== q 5))) empty-state)]]
              (car $))
            '((([0] . 5)) . 1))

(test-check second-set-t2
            (let [[$ ((call/fresh (lambda [q] (== q 5))) empty-state)]]
              (cdr $))
            '())

(test-check second-set-t3-take
            (let (($ (a-and-b empty-state)))
              (take 1 $))
            '(((([1] . 5) ([0] . 7)) . 2)))
