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

(experimental

(frame "3.7" [(unbound 0)]
       (run* q
             (listᵒ [:a :b q :d])))

(frame "3.10" [[]]
       (run 1 q
            (listᵒ (cons :a (cons :b (cons :c q))))))

(frame "3.14" [[]
               [(unbound 0)]
               [(unbound 0) (unbound 1)]
               [(unbound 0) (unbound 1) (unbound 2)]
               [(unbound 0) (unbound 1) (unbound 2) (unbound 3)]]
       (run 5 q
            (listᵒ (cons :a (cons :b (cons :c q))))))

(frame "3.20" [[]]
       (run 1 q
            (lolᵒ q)))

(frame "3.21" [true]
       (run* q
             (fresh [x y]
                    (lolᵒ [[:a :b] [x :c] [:d y]])
                    (≡ true q))))

(frame "3.22" [true]
       (run* q
             (fresh [x]
                    (lolᵒ (cons [:a :b] x))
                    (≡ true q))))

(frame "3.23" [[]]
       (run 1 q
            (lolᵒ (cons [:a :b] (cons [:c :d] q)))))

(frame "3.24" [[]
               [[]]
               [[] []]
               [[] [] []]
               [[] [] [] []]]
       (run 5 q
            (lolᵒ (cons [:a :b] (cons [:c :d] q)))))
)

(frame "3.32" [true]
       (run* q
             (twinsᵒ [:tofu :tofu])
             (≡ true q)))

(frame "3.33" [:tofu]
       (run* q
             (twinsᵒ [q :tofu])))

