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
        [adderall.internal [list*]])
(require adderall.dsl)
(require adderall.internal)

(defn-alias [nandᵒ nanod] [x y r]
  (condᵉ
   [(≡ 0 x) (≡ 0 y) (≡ 1 r)]
   [(≡ 1 x) (≡ 0 y) (≡ 1 r)]
   [(≡ 0 x) (≡ 1 y) (≡ 1 r)]
   [(≡ 1 x) (≡ 1 y) (≡ 0 r)]))

(defn-alias [xorᵒ xoro] [x y r]
  (fresh [s t u]
         (nandᵒ x y s)
         (nandᵒ x s t)
         (nandᵒ s y u)
         (nandᵒ t u r)))

(defn-alias [notᵒ noto] [x r]
  (nandᵒ x x r))

(defn-alias [andᵒ ando] [x y r]
  (fresh [s]
         (nandᵒ x y s)
         (notᵒ s r)))

(defn-alias [half-adderᵒ half-addero] [x y r c]
  (condᵉ
   [(≡ 0 x) (≡ 0 y) (≡ 0 r) (≡ 0 c)]
   [(≡ 1 x) (≡ 0 y) (≡ 1 r) (≡ 0 c)]
   [(≡ 0 x) (≡ 1 y) (≡ 1 r) (≡ 0 c)]
   [(≡ 1 x) (≡ 1 y) (≡ 0 r) (≡ 1 c)]))

(defn-alias [full-adderᵒ full-addero] [b x y r c]
  (fresh [w xy wz]
         (half-adderᵒ x y w xy)
         (half-adderᵒ w b r wz)
         (xorᵒ xy wz c)))

(defn build-num [n]
  (cond
   [(odd? n) (cons 1 (build-num (// (- n 1) 2)))]
   [(and (not (zero? n)) (even? n))
    (cons 0 (build-num (// n 2)))]
   [(zero? n) []]))

(defn-alias [posᵒ poso] [n]
  (fresh [a d]
         (≡ (cons a d) n)))

(defn-alias [>1ᵒ >1o] [n]
  (fresh [a ad dd]
         (≡ (list* a ad dd) n)))
