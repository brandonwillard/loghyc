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
        [tests.schemer.common [*]])
(require adderall.dsl)
(require tests.schemer.common)

(frame "6.6" []
       (run 1 [q]
            #uu
            neverᵒ))

(frame "6.7" [true]
       (run 1 [q]
            alwaysᵒ
            (≡ true q)))

(frame "6.10" [true true true true true]
       (run 5 [q]
            alwaysᵒ
            (≡ true q)))

(frame "6.11" [true true true true true]
       (run 5 [q]
            (≡ true q)
            alwaysᵒ))

(frame "6.13" [true]
       (run 1 [q]
            (salᵒ alwaysᵒ)
            (≡ true q)))

(frame "6.14" [true]
       (run 1 [q]
            (salᵒ neverᵒ)
            (≡ true q)))

(frame "6.19" [true]
       (run 1 [q]
            (condⁱ
             [(≡ false q) alwaysᵒ]
             [(≡ true q)])
            (≡ true q)))

(frame "6.21" [true true true true true]
       (run 5 [q]
            (condⁱ
             [(≡ false q) alwaysᵒ]
             [(anyᵒ (≡ true q))])
            (≡ true q)))

(defn teacupᵒ [x]
  (condᵉ
   [(≡ :tea x) #ss]
   [(≡ :cup x) #ss]
   [#uu]))

;; FIXME: This is disabled, because condⁱ does not behave properly yet
;; - it does not interleave.
#_(frame "6.24" [:tea false :cup]
         (run 5 [r]
              (condⁱ
               [(teacupᵒ r) #ss]
               [(≡ false r) #ss]
               [#ss #uu])))

(frame "6.25" [true true true true true]
       (run 5 [q]
            (condⁱ
             [(≡ false q) alwaysᵒ]
             [(≡ true q) alwaysᵒ]
             [#ss #uu])
            (≡ true q)))

(frame "6.28" [true true true true true]
       (run 5 [q]
            (condᵉ
             [alwaysᵒ #ss]
             [#ss neverᵒ])
            (≡ true q)))

#_(frame "6.32" [true]
         (run 1 [q]
              (allⁱ
               (condᵉ
                [(≡ false q) #ss]
                [#ss (≡ true q)])
               alwaysᵒ)
              (≡ true q)))

#_(frame "6.33" [true true true true true]
         (run 5 [q]
              (allⁱ
               (condᵉ
                [(≡ false q) #ss]
                [#ss (≡ true q)])
               alwaysᵒ)
              (≡ true q)))

#_(frame "6.34" [true true true true true]
         (run 5 [q]
              (allⁱ
               (condᵉ
                [(≡ true q) #ss]
                [#ss (≡ false q)])
               alwaysᵒ)
              (≡ true q)))

#_(frame "6.36" [true true true true true]
         (run 5 [q]
              (all
               (condᵉ
                [(≡ true q) #ss]
                [#ss (≡ false q)])
               alwaysᵒ)
              (≡ true q)))
