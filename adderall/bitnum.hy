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
        [adderall.internal [*]])

(require [adderall.dsl [*]])
(require [adderall.internal [defn-alias]])


(defn-alias [nandᵒ nando] [x y r]
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
   [(odd? n) (cons 1 (build-num (// (dec n) 2)))]
   [(and (not (zero? n)) (even? n))
    (cons 0 (build-num (// n 2)))]
   [(zero? n) []]))

(defn-alias [posᵒ poso] [n]
  (fresh [a d]
         (≡ (cons a d) n)))

(defn-alias [>1ᵒ >1o] [n]
  (fresh [a ad dd]
         (≡ (cons a ad dd) n)))

(defn-alias [adderᵒ addero] [d n m r]
  (condⁱ
   [(≡ 0 d) (≡ [] m) (≡ n r)]
   [(≡ 0 d) (≡ [] n) (≡ m r)
    (posᵒ m)]
   [(≡ 1 d) (≡ [] m)
    (adderᵒ 0 n [1] r)]
   [(≡ 1 d) (≡ [] n) (posᵒ m)
    (adderᵒ 0 [1] m r)]
   [(≡ [1] n) (≡ [1] m)
    (fresh [a c]
           (≡ [a c] r)
           (full-adderᵒ d 1 1 a c))]
   [(≡ [1] n) (gen-adderᵒ d n m r)]
   [(≡ [1] m) (>1ᵒ n) (>1ᵒ r)
    (adderᵒ d [1] n r)]
   [(>1ᵒ n) (gen-adderᵒ d n m r)]
   (else u#)))

(defn-alias [gen-adderᵒ gen-addero] [d n m r]
  (fresh [a b c e x y z]
         (≡ (cons a x) n)
         (≡ (cons b y) m) (posᵒ y)
         (≡ (cons c z) r) (posᵒ z)
         ;; (allⁱ
           (full-adderᵒ d a b c e)
           (adderᵒ e x y z)
         ;; )
         ))


(defn-alias [+ᵒ +o] [n m k]
  (adderᵒ 0 n m k))

(defn-alias [-ᵒ -o] [n m k]
  (+ᵒ m k n))

;; This is a terrible hack, because with python3, we don't do any
;; punycoding, therefore -ᵒ ends up being defined as _ᵒ, and functions
;; with leading underscores do not get automatically imported. (With
;; python2, we get mungling, and the function will start with a hy_
;; prefix, so we don't have that problem)
#_(if-python2
 None
 (setv EXPORTS ["nandᵒ" "nando"
                "xorᵒ" "xoro"
                "notᵒ" "noto"
                "andᵒ" "ando"
                "half_adderᵒ" "half_addero"
                "full_adderᵒ" "full_addero"
                "build_num"
                "posᵒ" "poso"
                ">1ᵒ" ">1o"
                "adderᵒ" "addero"
                "gen_adderᵒ" "gen_addero"
                "+ᵒ" "+o"
                "-ᵒ" "-o"]))
