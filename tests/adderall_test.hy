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

(defn r= [a b]
  (= (repr a) (repr b)))

(defn test-fail-and-succeed []
  (let [[q (fresh "q")]]
    (assert (= (run* q fail) []))
    (assert (r= (run* q succeed) [(LVar "_.0")]))))

(defn test-#s-and-#u []
  (let [[q (fresh "q")]]
    (assert (= (run* q #uu) []))
    (assert (r= (run* q #ss) [(LVar "_.0")]))))

(defn test-eq []
  (let [[q (fresh "q")]]
    (assert (r= (run* q (=ᵒ q q)) [(LVar "_.0")]))
    (assert (= (run* q (=ᵒ q True)) [True]))
    (assert (= (run* q (=ᵒ True q)) [True]))
    (assert (= (run* q (=ᵒ [1 2 3] q)) [[1 2 3]]))))

(defn test-fresh []
  (let [[q (fresh "q")]]
    (assert (r= (run* q (=ᵒ q (fresh "x"))) [(LVar "_.0")]))))

(defn test-bothᵍ []
  (let [[q (fresh "q")]]
    (assert (= (run* q (bothᵍ (=ᵒ q :tea) fail)) []))
    (assert (= (run* q (bothᵍ (=ᵒ q :tea) succeed)) [:tea]))
    (assert (= (run* q (let [[x (fresh "x")]]
                         (bothᵍ (=ᵒ x :tea) (=ᵒ x q)))) [:tea]))
    (assert (= (run* q (let [[x (fresh "x")]]
                         (bothᵍ (=ᵒ x :tea) (=ᵒ q x)))) [:tea]))
    (assert (= (run* q (let [[x (fresh "x")]]
                         (bothᵍ (=ᵒ q x)
                                (bothᵍ (=ᵒ x :tea)
                                       fail)))) []))
    (assert (= (run* q (let [[x (fresh "x")]]
                         (bothᵍ (=ᵒ q x)
                                (bothᵍ (=ᵒ x :tea)
                                       succeed)))) [:tea]))))

(defn test-eitherᵍ []
  (let [[q (fresh "q")]]
    (assert (r= (run* q (eitherᵍ succeed fail)) [(LVar "_.0")]))
    (assert (= (run* q (eitherᵍ fail fail)) []))
    (assert (= (run* q (eitherᵍ (=ᵒ q :tea) fail))
               [:tea]))
    (assert (= (run* q (eitherᵍ (=ᵒ q :tea) (=ᵒ q :coffee)))
               [:tea :coffee]))
    (assert (= (run* q (eitherᵍ (=ᵒ q :tea)
                                (eitherᵍ (=ᵒ q :coffee)
                                         (=ᵒ q :milk))))
               [:tea :coffee :milk]))
    (assert (= (run* q (eitherᵍ (=ᵒ q :tea)
                                (=ᵒ q :coffee)
                                (=ᵒ q :milk)))
               [:tea :coffee :milk]))))

(defn test-eitherᵍ-and-bothᵍ []
  (let [[q (fresh "q")]]
    (assert (= (run* q (eitherᵍ (=ᵒ q :tea)
                                (bothᵍ (=ᵒ q :coffee)
                                       succeed)))
               [:tea :coffee]))
    (assert (= (run* q (eitherᵍ (=ᵒ q :tea)
                                (bothᵍ (=ᵒ q :coffee)
                                       fail)))
               [:tea]))
    (assert (= (run* q (eitherᵍ fail
                                (bothᵍ (=ᵒ q :coffee)
                                       (=ᵒ q :tea))))
               []))
    (assert (r= (run* q (eitherᵍ succeed
                                 (bothᵍ (=ᵒ q :coffee)
                                        (=ᵒ q :tea))))
                [(LVar "_.0")]))
    (assert (r= (run* q (let [[x (fresh "x")]
                              [y (fresh "y")]]
                          (eitherᵍ (bothᵍ (=ᵒ q x) (=ᵒ x x))
                                   (bothᵍ (=ᵒ q y) (=ᵒ y y)))))
                [(LVar "_.0") (LVar "_.0")]))))

(defn test-allᵍ []
  (let [[q (fresh "q")]]
    (assert (= (run* q (allᵍ (=ᵒ q :coffee)
                             succeed
                             succeed))
               [:coffee]))
    (assert (= (run* q (allᵍ (=ᵒ q :coffee)
                             succeed
                             fail))
               []))
    (assert (= (run* q (allᵍ (=ᵒ q :coffee)
                             (=ᵒ q :tea)
                             (=ᵒ q :milk)))
               []))
    (assert (= (run* q (let [[x (fresh "x")]
                             [y (fresh "y")]]
                         (allᵍ (=ᵒ x :coffee)
                               (=ᵒ y :tea)
                               (eitherᵍ (=ᵒ q x)
                                        (=ᵒ q y)
                                        (=ᵒ q :milk)))))
               [:coffee :tea :milk]))))

(defn test-run* []
  (let [[q (fresh "q")]
        [x (fresh "x")]
        [y (fresh "y")]]
    (assert (= (run* q
                     (=o x :coffee)
                     (=o y :tea)
                     (eitherg (=o q x)
                              (=o q y)
                              (=o q :milk)))
               [:coffee :tea :milk]))))
