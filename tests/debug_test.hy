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
        [io [StringIO]]
        [sys])
(require adderall.dsl)
(require adderall.debug)

(defmacro/g! wrap-stdout [&rest body]
  `(do
    (import [sys] [StringIO [StringIO]])
    (setv ~g!old-stdout sys.stdout)
    (setv sys.stdout (StringIO))
    (setv ~g!result (do ~@body))
    (setv ~g!stdout (.getvalue sys.stdout))
    (setv sys.stdout ~g!old-stdout)
    [~g!stdout ~g!result]))

(defn test-log []
  (assert (= (wrap-stdout
              (run* [q]
                    (log "hello")
                    (≡ q true)))
             ["hello\n" [true]])))

(defn test-trace-s []
  (assert (= (wrap-stdout
              (run* [q]
                    (trace-s)
                    (≡ q true)))
             ["()\n" [true]])))
