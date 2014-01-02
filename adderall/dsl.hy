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

(import [itertools [islice]]
        [adderall.internal [*]]
        [adderall.lvar [LVar]])

;; Top level stuff

(defn run [var goal &optional [n nil]]
  (let [[solutions (gen-solutions var goal)]]
    (if (= n nil)
      (list solutions)
      (islice solutions 0 n))))

(defn fresh [names_str]
  (setv names (.split names_str))
  (if (= (len names) 1)
    (LVar (get names 0))
    (map LVar names)))

;; Goals

(defn eq [u v]
  (fn [s]
    (yield (unify u v s))))

(defn succeed [s]
  (yield s))

(defn fail [s]
  (iter ()))

(defn both [g1 g2]
  (fn [s]
    (for [opt-s1 (g1 s)]
      (when (not (is opt-s1 None))
        (for [opt-s2 (g2 opt-s1)]
          (yield opt-s2))))))
