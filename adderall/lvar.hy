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

(defclass LVar [object]
  [[--init-- (fn [self name &optional unbound]
               (setv self.name name)
               (when unbound
                 (setv self.unbound true))
               nil)]
   [--hash-- (fn [self]
               (hash self.name))]
   [--eq--   (fn [self other]
               (and (= (type self) (type other))
                    (= self.name other.name)))]
   [--repr-- (fn [self]
               (.format "<{0!r}>" self.name))]
   [bound? (fn [self]
             (if self.unbound
               true
               false))]])

(defn unbound [n]
  (LVar (.format "_.{0}" n) :unbound))
