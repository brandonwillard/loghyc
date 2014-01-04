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
        [hy.models.symbol [HySymbol]])
(require adderall.dsl)

(def unbound (LVar "_.0"))

(defmacro frame [frame-num value expr]
  (let [[res (gensym)]
        [name (+ 'test-rs- frame-num)]]
    `(defn ~(HySymbol name) []
       (let [[q (fresh [q])]
             [~res ~expr]]
         (assert (= ~value ~res))))))

(frame "1.10" []
       (run* q
             #uu))

(frame "1.11" [True]
       (run* q
             (=ᵒ True q)))

(frame "1.12" []
       (run* q
             #uu
             (=ᵒ True q)))

(frame "1.13" [True]
       (run* q
             #ss
             (=ᵒ True q)))

(frame "1.15" ['corn]
       (run* q
             #ss
             (=ᵒ 'corn q)))

(frame "1.17" []
       (run* q
             #uu
             (=ᵒ 'corn q)))

(frame "1.18" [False]
       (run* q
             #ss
             (=ᵒ False q)))

(frame "1.22" []
       (run* q
             (let [[x False]]
               (=ᵒ True x))))

(frame "1.23" [True]
       (run* q
             (fresh [x]
                    (=ᵒ True x)
                    (=ᵒ True q))))

(frame "1.26" [True]
       (run* q
             (fresh [x]
                    (=ᵒ x True)
                    (=ᵒ True q))))

(frame "1.26" [True]
       (run* q
             (fresh [x]
                    (=ᵒ x True)
                    (=ᵒ q True))))

(frame "1.28" [unbound]
       (run* q
             #ss))

(frame "1.29" [unbound]
       (run* q
             (let [[q False]]
               (fresh [q]
                      (=ᵒ True q)))))
