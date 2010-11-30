;;;  cartesian-product (list1 list2)
;;          Retorna lista composta pelo produto cartesiano de duas listas.
;;
;;   Copyright (C) 2010  David Fernandes
;;                       <daugfernandes@aim.com>
;;
;;   This program is free software: you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation, either version 3 of the License, or
;;   (at your option) any later version.
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with this program named license.txt.
;;   If not, see <http://www.gnu.org/licenses/>

(defun cartesian-product(L1 L2)
  (if
    (and (not (null L1)) (not (null L2)))
    (append
      (append 
        (list (list (first L1) (first L2)))
        (cartesian-product (list (first L1)) (rest L2))
      )
      (cartesian-product (rest L1) L2)
    )
  )
)

