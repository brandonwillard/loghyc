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

(import [nose.tools [assert-equal assert-not-equal]]
        [cons [cons car cdr]]
        [adderall.dsl [*]]
        [adderall.internal [*]])

(require [adderall.dsl [*]])
(require [adderall.debug [*]])
(require [tests.utils [*]])
(require [hy.contrib.walk [let]])

(defn assert-all-equal [&rest tests]
  (reduce (fn [x y] (assert-equal x y) y)
          tests)
  None)

(defn test-cons []
  (assert-all-equal
    ;;(cons 'a None)
    (cons 'a [])
    ['a])
  (assert-all-equal
    (cons 'a (,))
    (,'a))
  (assert-all-equal
    (cons 'a '())
    '(a))
  
  (assert-all-equal
    (cons 'a ['b 'c])
    ['a 'b 'c])
  
  (assert-all-equal
    (cons 'a (, 'b 'c))
    (,'a 'b 'c))
  (assert-all-equal
    (cons 'a '(b c))
    '(a b c))

  (assert-not-equal (cons 'a (, 'b 'c))
                    (, 'a))
  (assert-all-equal
    (cons '(a b) '(c d))
    '((a b) c d))
  
  (assert-all-equal
    (cons 'a (, 'b 'c))
    (, 'a 'b 'c)
    )
  
  (assert-all-equal
    (cons '(a b) '(c))
    '((a b) c)
    )
  (assert-all-equal
    (cons ['a 'b] ['c])
    [['a 'b] 'c]
    )
  (assert-all-equal
    (cons (, 'a 'b) (, 'c))
    (, (, 'a 'b) 'c)
    )
  (assert-equal (car (cons 'a 'b))
                'a)

  (assert-all-equal
    (car (cons '(a b) 'a))
    '(a b))
  (assert-all-equal
    (car (cons ['a 'b] 'a))
    ['a 'b])
  (assert-all-equal
    (car (cons (, 'a 'b) 'a))
    (, 'a 'b) )
  
  (assert-all-equal (car (cons (, 'a 'b) 'a))
                    (, 'a 'b))

  (assert-equal (cdr (cons 'a 'b))
                'b)

  (assert-all-equal
    (cdr (cons 'a '()))
    '())
  (assert-all-equal
    (cdr (cons 'a (,)))
    (,))
  (assert-all-equal
    (cdr (cons 'a []))
    [])

  (assert-all-equal
    (cdr (cons 'a '(b)))
    (cdr (cons '(a) '(b)))
    '(b))
  (assert-all-equal
    (cdr (cons 'a ['b]))
    ['b])
  (assert-all-equal
    (cdr (cons 'a (, 'b)))
    (, 'b))


  (assert-all-equal (cdr (cons 'a (, 'b)))
                    (, 'b))
)

(defn test-fail-and-succeed []
  (assert-equal (run* [q] fail) [])
  (assert-equal (run* [q] succeed) [#U 0]))

(defn test-s#-and-u# []
  (assert-equal (run* [q] fail) [])
  (assert-equal (run* [q] succeed) [#U 0]))

(defn test-fresh []
  (assert-equal (run* [q] (fresh [x])) [#U 0]))

(defn test-consᵒ []
  (assert-equal (run* [q] (consᵒ 1 [2 3] [1 2 3]))
                [#U 0])
  (assert-equal (run* [q] (consᵒ q [2 3] [1 2 3]))
                [1])
  (assert-equal (run* [q] (consᵒ q 2 (cons 1 2)))
                [1])
  (assert-equal (run* [q] (consᵒ q 2 '(1 2)))
                [])
  (assert-equal (run* [q] (consᵒ 1 q [1 2 3]))
                [[2 3]])
  (assert-equal (run* [q] (consᵒ 1 [2 3] q))
                [(cons 1 [2 3])]
                [[1 2 3]])
  (assert-equal (run* [q] (consᵒ q '(b) '(a b)))
                ['a])
  (assert-equal (run* [q] (consᵒ 1 [q 3] [1 2 3]))
                [2])
  (assert-equal (run* [q] (consᵒ 1 [2 q] [1 2 3]))
                [3])
  (assert-equal (run* [q] (consᵒ 1 [2 3] [q 2 3]))
                [1])
  (assert-equal (run* [q] (consᵒ 1 [2 3] [1 q 3]))
                [2])
  (assert-equal (run* [q] (consᵒ 1 [2 3] [1 2 q]))
                [3])
  (assert-equal (run* [q] (consᵒ 1 2 q))
                [(cons 1 2)]))

(defn test-project []
  (assert-equal (run* [q] (fresh [x]
                                 (≡ x 2)
                                 (≡ q (type x))))
                [LVar])
  (assert-equal (run* [q] (fresh [x] (≡ x 2)
                                 (project [x]
                                          (≡ q (type x)))))
                [(type 2)]))

(defn test-prep []
  (assert-equal (run* [q] (prep
                            (≡ q ?x)
                            (memberᵒ ?x [?y 4 2])
                            (memberᵒ ?y [1 3 5])))
                [1 3 5 4 4 4 2 2 2]))

;; (defn test-set-support []
;;   (assert-equal (run* [q]
;;                       (memberᵒ q (set [1 2 3 1 2 3])))
;;                 [1 2 3])
;;   (assert-equal (run* [q]
;;                       (memberᵒ 3 (set [1 2 q q])))
;;                 [3]))

(defn test-lazyness []
  (assert-equal (first (wrap-stdout
                         (lazy-run* [q]
                                    (log "hello")
                                    (≡ q True))))
                "")

  (assert-equal (first (wrap-stdout
                         (list (lazy-run* [q]
                                          (log "hello")
                                          (≡ q True)))))
                "hello\n")

  (assert-equal (first (wrap-stdout
                         (run* [q]
                               (log "hello")
                               (≡ q True))))
                "hello\n"))

(defn test-custom-unification []
  (defclass unifyClass [object])
  (let [l (unifyClass)]
       (setv l.unify (fn [u v s] (, u 1 s)))
       (assert-equal (run* [q]
                           (≡ l q))
                     [1])

       (setv l.unify None)
       (assert-equal (run* [q]
                           (≡ l q))
                     [l])))

(defn test-run1 []
  (assert-equal (run¹ [q] (≡ q 1)) 1)
  (assert-equal (run¹ [q] (≡ q 1) (≡ q 2)) None)
  (assert-equal (run 1 [q] (≡ q 1)) [1]))
