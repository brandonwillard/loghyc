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

;;
;; This is for solving the [Zebra Puzzle][zebra].
;;
;;  [zebra]: https://en.wikipedia.org/wiki/Zebra_Puzzle
;;

(import [adderall.dsl [*]])
(import [adderall.internal [*]])

(require [adderall.dsl [*]])
(require [adderall.internal [*]])

;; Goals

(defn-alias [rightᵒ righto] [x y l]
  (condᵉ
     [(fresh [r]
             (≡ (cons x y r) l))]
     [(fresh [r]
             (restᵒ l r)
             (rightᵒ x y r))]))

(defn-alias [nextᵒ nexto] [x y l]
  (condᵉ
   [(rightᵒ x y l)]
   [(rightᵒ y x l)]))

(defmacro-alias [zebra-tableᵖ zebra-tablep] [hs]
  `(all
    (≡ [#? _ #? _ [#? _ #? _ 'milk #? _ #? _] #? _ #? _] ~hs)
    (firstᵒ ~hs ['norwegian #? _ #? _ #? _ #? _])
    (nextᵒ ['norwegian #? _ #? _ #? _ #? _]
           [#? _        #? _ #? _ #? _ 'blue] ~hs)
    (rightᵒ [#? _ #? _ #? _ #? _ 'ivory]
            [#? _ #? _ #? _ #? _ 'green]
            ~hs)
    (memberᵒ ['englishman #? _            #? _     #? _     'red] ~hs)
    (memberᵒ [#? _         'kools         #? _     #? _     'yellow] ~hs)
    (memberᵒ ['spaniard   #? _            #? _     'dog    #? _] ~hs)
    (memberᵒ [#? _         #? _            'coffee #? _     'green] ~hs)
    (memberᵒ ['ukrainian  #? _            'tea    #? _     #? _] ~hs)
    (memberᵒ [#? _         'lucky-strikes 'oj     #? _     #? _] ~hs)
    (memberᵒ ['japanese   'parliaments   #? _     #? _     #? _] ~hs)
    (memberᵒ [#? _         'oldgods       #? _     'snails #? _] ~hs)
    (nextᵒ [#? _ #? _    #? _ 'horse #? _]
           [#? _ 'kools #? _ #? _    #? _]
           ~hs)
    (nextᵒ [#? _ #? _            #? _ 'fox #? _]
           [#? _ 'chesterfields #? _ #? _  #? _]
           ~hs)))

(defmacro-alias [zebraᵖ zebrap] [w h]
  `(fresh [hs]
          (zebra-tableᵖ hs)
          (memberᵒ [~w #? _ 'water #? _    #? _] hs)
          (memberᵒ [~h #? _ #? _    'zebra #? _] hs)))
