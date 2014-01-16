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

(def unbound (LVar "_.0"))

(defn test-fail-and-succeed []
  (assert (= (run* [q] fail) []))
  (assert (= (run* [q] succeed) [unbound])))

(defn test-#s-and-#u []
  (assert (= (run* [q] #uu) []))
  (assert (= (run* [q] #ss) [unbound])))

(defn test-eq []
  (assert (= (run* [q] (≡ q q)) [unbound]))
  (assert (= (run* [q] (≡ q true)) [true]))
  (assert (= (run* [q] (≡ true q)) [true]))
  (assert (= (run* [q] (≡ [1 2 3] q)) [[1 2 3]]))
)

(defn test-fresh []
  (assert (= (run* [q] (≡ q (fresh [x]))) [unbound])))

(defn test-bothᵍ []
  (assert (= (run* [q] (bothᵍ (≡ q :tea) fail)) []))
  (assert (= (run* [q] (bothᵍ (≡ q :tea) succeed)) [:tea]))
  (assert (= (run* [q] (let [[x (fresh [x])]]
                         (bothᵍ (≡ x :tea) (≡ x q)))) [:tea]))
  (assert (= (run* [q] (let [[x (fresh [x])]]
                         (bothᵍ (≡ x :tea) (≡ q x)))) [:tea]))
  (assert (= (run* [q] (let [[x (fresh [x])]]
                         (bothᵍ (≡ q x)
                                (bothᵍ (≡ x :tea)
                                       fail)))) []))
  (assert (= (run* [q] (let [[x (fresh [x])]]
                         (bothᵍ (≡ q x)
                                (bothᵍ (≡ x :tea)
                                       succeed)))) [:tea])))

(defn test-eitherᵍ []
  (assert (= (run* [q] (eitherᵍ succeed fail)) [unbound]))
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

(defn test-eitherᵍ-and-bothᵍ []
  (assert (= (run* [q] (eitherᵍ (≡ q :tea)
                                (bothᵍ (≡ q :coffee)
                                       succeed)))
             [:tea :coffee]))
  (assert (= (run* [q] (eitherᵍ (≡ q :tea)
                                (bothᵍ (≡ q :coffee)
                                       fail)))
             [:tea]))
  (assert (= (run* [q] (eitherᵍ fail
                                (bothᵍ (≡ q :coffee)
                                       (≡ q :tea))))
             []))
  (assert (= (run* [q] (eitherᵍ succeed
                                (bothᵍ (≡ q :coffee)
                                       (≡ q :tea))))
             [unbound]))
  (assert (= (run* [q] (let [[[x y] (fresh [x y])]]
                         (eitherᵍ (bothᵍ (≡ q x) (≡ x x))
                                  (bothᵍ (≡ q y) (≡ y y)))))
             [unbound unbound])))

(defn test-allᵍ []
  (assert (= (run* [q] (allᵍ (≡ q :coffee)
                             succeed
                             succeed))
             [:coffee]))
  (assert (= (run* [q] (allᵍ (≡ q :coffee)
                             succeed
                             fail))
             []))
  (assert (= (run* [q] (allᵍ (≡ q :coffee)
                             (≡ q :tea)
                             (≡ q :milk)))
             []))
  (assert (= (run* [q] (let [[[x y] (fresh [x y])]]
                         (allᵍ (≡ x :coffee)
                               (≡ y :tea)
                               (eitherᵍ (≡ q x)
                                        (≡ q y)
                                        (≡ q :milk)))))
             [:coffee :tea :milk])))

(defn test-run* []
  (let [[[x y] (fresh [x y])]]
    (assert (= (run* [q]
                     (≡ x :coffee)
                     (≡ y :tea)
                     (eitherᵍ (≡ q x)
                              (≡ q y)
                              (≡ q :milk)))
               [:coffee :tea :milk])))
  (assert (= (run* [q x]
                   (≡ x :coffee)
                   (≡ q :milk))
             [[:milk :coffee]])))

(defn test-run []
  (let [[[x y] (fresh [x y])]]
    (assert (= (run 1 [q]
                    (≡ x :coffee)
                    (≡ y :tea)
                    (eitherᵍ (≡ q x)
                             (≡ q y)
                             (≡ q :milk)))
               [:coffee]))
    (assert (= (run 2 [q]
                    (≡ x :coffee)
                    (≡ y :tea)
                    (eitherᵍ (≡ q x)
                             (≡ q y)
                             (≡ q :milk)))
               [:coffee :tea]))
    (assert (= (run 4 [q]
                    (≡ x :coffee)
                    (≡ y :tea)
                    (eitherᵍ (≡ q x)
                             (≡ q y)
                             (≡ q :milk)))
               [:coffee :tea :milk]))))

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
             [unbound :tea])))

(defn test-consᵒ []
  (assert (= (run* [q] (conso 1 [2 3] [1 2 3]))
             [unbound]))
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
