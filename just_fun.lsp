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

(defun fib(n)
  (if (<= n 2)
    1
    (+ (fib (- n 1)) (fib (- n 2)))
  )
)


; com duas pequenas adaptações a coisa fica mais aberta
(defun tr (L funcao)  
  (if (null L)
    nil
    (append 
      (list (funcall funcao L))
      (tr (cdr L) funcao)
    )
  )
)

; dobro
(tr (list 1 2 3 4 5) (lambda (x) (* (car x) 2)))
; triplo
(tr (list 1 2 3 4 5) (lambda (x) (* (car x) 3)))
; a raíz quadrada
(tr (list 1 2 3 4 5) (lambda (x) (sqrt (car x))))

;; as seguintes, como tratam elementos consecutivos, no último, retorna ele
;; próprio. como exemplo serve.

; a média dos elementos dois a dois (sendo que o ultimo é ele próprio :( )
(tr (list 1 2 3 4 5) (lambda (x) (if (null (car (cdr x))) (car x) (/ (+ (car x) (car (cdr x))) 2))))
; o mmc entre dois elementos consecutivos (sendo que o ultimo é ele próprio :( )
(tr (list 2 4 12 4) (lambda (x) (if (null (car (cdr x))) (car x) (lcm (car x) (car (cdr x))))))
; o menor de cada dois elementos consecutivos (sendo que o ultimo é ele próprio :( )
(tr (list 1 5 3 12 24 48 9) (lambda (x) (if (null (car (cdr x))) (car x) (if (< (car x) (car (cdr x))) (car x) (car (cdr x))))))

(defun random-list (ncons min max)
   (if (<= ncons 1)
      (list (random (+ (1+ (- max min)) min)))
      (cons (random (+ (1+ (- max min)) min)) (random-list (1- ncons) min max))
   )
)
