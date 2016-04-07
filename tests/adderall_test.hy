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

(import [adderall.dsl [*]])
(require adderall.dsl)
(require adderall.debug)
(require tests.utils)

(defn test-fail-and-succeed []
  (assert (= (run* [q] fail) []))
  (assert (= (run* [q] succeed) [#U0])))

(defn test-#s-and-#u []
  (assert (= (run* [q] #uu) []))
  (assert (= (run* [q] #ss) [#U0])))

(defn test-fresh []
  (assert (= (run* [q] (fresh [x])) [#U0])))

(defn test-consᵒ []
  (assert (= (run* [q] (consᵒ 1 [2 3] [1 2 3]))
             [#U0]))
  (assert (= (run* [q] (consᵒ q [2 3] [1 2 3]))
             [1]))
  (assert (= (run* [q] (consᵒ 1 q [1 2 3]))
             [[2 3]]))
  (assert (= (run* [q] (consᵒ 1 [2 3] q))
             [[1 2 3]]))
  (assert (= (run* [q] (consᵒ 1 [q 3] [1 2 3]))
             [2]))
  (assert (= (run* [q] (consᵒ 1 [2 q] [1 2 3]))
             [3]))
  (assert (= (run* [q] (consᵒ 1 [2 3] [q 2 3]))
             [1]))
  (assert (= (run* [q] (consᵒ 1 [2 3] [1 q 3]))
             [2]))
  (assert (= (run* [q] (consᵒ 1 [2 3] [1 2 q]))
             [3]))
  (assert (= (run* [q] (consᵒ 1 2 q))
             [(cons 1 2)])))

(defn test-project []
  (assert (= (run* [q] (fresh [x]
                              (≡ x 2)
                              (≡ q (type x))))
             [LVar]))
  (assert (= (run* [q] (fresh [x]
                              (≡ x 2)
                              (project [x]
                                       (≡ q (type x)))))
             [(type 2)])))

(defn test-prep []
  (assert (= (run* [q] (prep
                        (≡ q ?x)
                        (memberᵒ ?x [?y 4 2])
                        (memberᵒ ?y [1 3 5])))
             [1 3 5 4 4 4 2 2 2])))

(defn test-set-support []
  (assert (= (run* [q]
                   (memberᵒ q (set [1 2 3 1 2 3])))
             [1 2 3]))
  (assert (= (run* [q]
                   (memberᵒ 3 (set [1 2 q q])))
             [3])))

(defn test-lazyness []
  (assert (= (first (wrap-stdout
                     (lazy-run* [q]
                                (log "hello")
                                (≡ q true))))
             ""))

  (assert (= (first (wrap-stdout
                     (list (lazy-run* [q]
                                      (log "hello")
                                      (≡ q true)))))
             "hello\n"))

  (assert (= (first (wrap-stdout
                     (run* [q]
                           (log "hello")
                           (≡ q true))))
             "hello\n")))

(defn test-custom-unification []
  (defclass unifyClass [object])
  (let [l (unifyClass)]
    (setv l.unify (fn [u v s] (, u 1 s)))
    (assert (= (run* [q]
                     (≡ l q))
               [1]))

    (setv l.unify nil)
    (assert (= (run* [q]
                     (≡ l q))
               [l]))))

(defn test-run1 []
  (assert (= (run1 [q] (≡ q 1)) 1))
  (assert (= (run1 [q] (≡ q 1) (≡ q 2)) nil))
  (assert (= (run 1 [q] (≡ q 1)) [1])))
