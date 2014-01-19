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

(defn test-eq []
  (assert (= (run* [q] (≡ q q)) [#U0]))
  (assert (= (run* [q] (≡ q true)) [true]))
  (assert (= (run* [q] (≡ true q)) [true]))
  (assert (= (run* [q] (≡ [1 2 3] q)) [[1 2 3]])))

(defn test-fresh []
  (assert (= (run* [q] (fresh [x])) [#U0])))

(defn test-eitherᵍ []
  (assert (= (run* [q] (eitherᵍ succeed fail)) [#U0]))
  (assert (= (run* [q] (eitherᵍ fail fail)) []))
  (assert (= (run* [q] (eitherᵍ (≡ q :tea) fail))
             [:tea]))
  (assert (= (run* [q] (eitherᵍ (≡ q :tea) (≡ q :coffee)))
             [:tea :coffee]))
  (assert (= (run* [q] (eitherᵍ (≡ q :tea)
                                (eitherᵍ (≡ q :coffee)
                                         (≡ q :milk))))
             [:tea :coffee :milk]))
  (assert (= (run* [q] (eitherᵍ (≡ q :tea)
                                (≡ q :coffee)
                                (≡ q :milk)))
             [:tea :coffee :milk])))

(defn test-run* []
  (assert (= (run* [q]
                     (fresh [x y]
                            (≡ x :coffee)
                            (≡ y :tea)
                            (eitherᵍ (≡ q x)
                                     (≡ q y)
                                     (≡ q :milk))))
               [:coffee :tea :milk]))
  (assert (= (run* [q x]
                   (≡ x :coffee)
                   (≡ q :milk))
             [[:milk :coffee]])))

(defn test-run []
  (assert (= (run 1 [q]
                  (fresh [x y]
                         (≡ x :coffee)
                         (≡ y :tea)
                         (eitherᵍ (≡ q x)
                                  (≡ q y)
                                  (≡ q :milk))))
             [:coffee]))
  (assert (= (run 2 [q]
                  (fresh [x y]
                         (≡ x :coffee)
                         (≡ y :tea)
                         (eitherᵍ (≡ q x)
                                  (≡ q y)
                                  (≡ q :milk))))
             [:coffee :tea]))
  (assert (= (run 4 [q]
                  (fresh [x y]
                         (≡ x :coffee)
                         (≡ y :tea)
                         (eitherᵍ (≡ q x)
                                  (≡ q y)
                                  (≡ q :milk))))
             [:coffee :tea :milk])))

(defn test-fresh-with-goals []
  (assert (= (run* [q] (fresh [x y]
                              (≡ x :coffee)
                              (≡ y :tea)
                              (eitherᵍ (≡ q x)
                                       (≡ q y)
                                       (≡ q :milk))))
             [:coffee :tea :milk])))

(defn test-condᵉ []
  (assert (= (run* [q] (condᵉ
                        [succeed (≡ q :coffee)]
                        [fail (≡ q :tea)]))
             [:coffee]))
  (assert (= (run* [q] (fresh [x y]
                              (≡ x :good)
                              (≡ y :good)
                              (condᵉ
                               [(≡ x :good) (≡ q :coffee)]
                               [(≡ y :good) (≡ q :tea)])))
             [:coffee :tea]))
  (assert (= (run* [q] (fresh [x y]
                              (≡ x :best)
                              (≡ y :good)
                              (condᵉ
                               [(≡ x :good) (≡ q :coffee)]
                               [(≡ y :good) (≡ q :tea)])))
             [:tea]))
  (assert (= (run* [q] (fresh [x y]
                              (≡ x 1)
                              (condᵉ
                               [(≡ x 1) (≡ y 2)]
                               [(≡ x 1) (≡ q :tea)])))
             [#U0 :tea])))

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
