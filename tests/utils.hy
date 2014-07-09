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

(defmacro/g! wrap-stdout [&rest body]
  `(do
    (import [sys] [io [StringIO]])
    (setv ~g!old-stdout sys.stdout)
    (setv sys.stdout (StringIO))
    (setv ~g!result (do ~@body))
    (setv ~g!stdout (.getvalue sys.stdout))
    (setv sys.stdout ~g!old-stdout)
    [~g!stdout ~g!result]))
