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

(import [adderall.dsl [*]])

(require [adderall.dsl [*]])
(require [adderall.internal [defn-alias]])


(defn-alias [typeᵒ typeo] [u v]
  (condᵉ
   [(project [u] (if (= (type u) LVar)
                   fail
                   (≡ v (type u))))]
   [(project [v] (if (= (type v) LVar)
                   fail
                   (≡ u (type v))))]
   (else (project [u v]
                  (≡ (type u) (type v))))))
