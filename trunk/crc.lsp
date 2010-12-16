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


; value e polinom devem ser números base 10
; - value é o valor a codificar,
; - polinom é o valor decimal dos coeficientes do polinomio gerador (x^3 + 1 => 1001 => 9)
(defun crc (value polinom)
  (let* ((c (* value (expt 2 (values (floor (log polinom 2)))))))
    (values
      (+ (mod c polinom) c)
      (mod value polinom)
    )
  )
)

;; tests if an integer is made of 0 and 1's
(defun int-is-bin (i)
  (let ((m (mod i 10)))  
    (and
      (< m 2)
      (or 
        (< i 2) 
        (int-is-bin (/ (- i m) 10))
      )
    )
  )
)

