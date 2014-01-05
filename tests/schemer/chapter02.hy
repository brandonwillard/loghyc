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
        [tests.schemer.common [*]])
(require adderall.dsl)
(require tests.schemer.common)

(frame "2.6" [:a]
       (run* q
             (firstᵒ (lcons :a (lcons :c (lcons :o (lcons :r (lcons :n nil))))) q)))

(frame "2.7" [true]
       (run* q
             (firstᵒ [:a :c :o :r :n] :a)
             (=ᵒ true q)))

(frame "2.8" [:pear]
       (run* q
             (fresh [x y]
                    (firstᵒ [q y] x)
                    (=ᵒ :pear x))))

;; 2.11 not implemented, because there's no native cons in Hy

(frame "2.15" [:c]
       (run* q
             (fresh [v]
                    (restᵒ [:a :c :o :r :n] v)
                    (firstᵒ v q))))

;; 2.18 skipped, because of no cons in Hy

(frame "2.19" [true]
       (run* q
             (restᵒ [:a :c :o :r :n] [:c :o :r :n])
             (=ᵒ true q)))
