;;;  s7-list
;;
;;   Functions/macros to deal with lists
;;
;; ----------------------------------------------------------------------
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
;;
;; ----------------------------------------------------------------------

(defun list-first-n (L n)
  (if (and L (> n 0))
      (cons (car L) (list-first-n (cdr L) (1- n)))
  )
)

;; a list-first-n with reversed counter
(defun list-except-last-n (L n)
  (list-first-n L (- (length L) n))
)

;; a recursive (yet slower) version of (nthcdr)
(defun list-except-first-n (L n)
  (if (<= n 0)
      L
      (list-except-first-n (cdr L) (1- n))
  )
)

(defun list-aggregate (L select-function)
  (if (null (cdr L))
    (car L)
    (funcall select-function (car L) (list-aggregate (list-except-first-n L 1) select-function))
  )
)

(defun list-unique (L)
  (if (null L)
    nil
    (if (equal (first L) (second L))
        (list-unique (rest L))
        (cons (first L) (list-unique (rest L)))
    )
  )
)

(defun random-list (ncons min max)
   (if (<= ncons 1)
      (list (random (+ (1+ (- max min)) min)))
      (cons (random (+ (1+ (- max min)) min)) (random-list (1- ncons) min max))
   )
)

;; get-param[-list]
;
;  numa lista especialmente composta por pares de elementos 
;  (key_1 valor_1 ... key_n valor_n) procura key_i = param 
;  e retorna valor_i (ou nil se não encontrar)
;
;  em duas versões:
;    get-param      retorna apenas um elemento
;    get-param-list retorna SEMPRE uma lista (de 1 ou mais elementos)
;
;  exemplos de utilização:
;
;     (setq L (list :nome "david" :idade 43 :sexo "M"))
;
;     ; uso similar a 'getf'
;     (get-param L :nome) => "david"
;
;     (setq L (list "a" 10 "b" 30 "c" 40 "b" 1.4))
;     (get-param L "b") => 30
;     (get-param-list L "a") => (10)
;     (get-param-list L "b") => (30 2.4)
;

(defun get-param (list param)
  (if
    (null list)
    nil
    (if
      (equal (car list) param)
      (car (cdr list))
      (get-param (nthcdr 2 list) param)
    )
  )
)

(defun get-param-list (list param)
  (if 
    (null list)
    nil
    (if 
      (equal (car list) param)
      (cons (car (cdr list)) (get-param-list (nthcdr 2 list) param))
      (get-param-list (nthcdr 2 list) param)
    )
  )
)

;; destructive
(defmacro push-tail (element L)
  `(if (null ,L)
    (setf ,L (cons ,element nil))
    (push-tail ,element (cdr ,L))
  ) 
)

;; destructive
(defmacro pop-tail (L)
  `(if (null (cdr ,L))
    (let ((tmp (car ,L)))
      (setf ,L nil)
      tmp
    )
    (pop-tail (cdr ,L))
  ) 
)

;;--------------------------------------------------------------------------
;; unit-tests
;;

(defun s7-list-ut ()

  (assert (equal (list-first-n (list 1 2 3 4 5) 1) (list 1)))
  (assert (null  (list-first-n (list 1 2 3 4 5) 0)))
  (assert (equal (list-first-n (list 1 2 3 4 5 6 7 8 9) 6) (list 1 2 3 4 5 6)))
  (assert (equal (list-first-n (list 1 2 3 4 5 6 7 8 9) 226) (list 1 2 3 4 5 6 7 8 9)))

  (assert (equal (list-except-last-n (list 1 2 3 4 5) 4) (list 1)))
  (assert (null  (list-except-last-n (list 1 2 3 4 5) 5)))
  (assert (equal (list-except-last-n (list 1 2 3 4 5 6 7 8 9) 3) (list 1 2 3 4 5 6)))
  (assert (equal (list-except-last-n (list 1 2 3 4 5 6 7 8 9) 0) (list 1 2 3 4 5 6 7 8 9)))

  (assert (equal (list-except-first-n (list 1 2 3 4 5) 4) (list 5)))
  (assert (null  (list-except-first-n (list 1 2 3 4 5) 5)))
  (assert (equal (list-except-first-n (list 1 2 3 4 5 6 7 8 9) 3) (list 4 5 6 7 8 9)))
  (assert (equal (list-except-first-n (list 1 2 3 4 5 6 7 8 9) 0) (list 1 2 3 4 5 6 7 8 9)))

  (assert (equal (list-aggregate (list 1 2 3 4 6 2 100 2 3 4 0 3 4 2 3 4 5) #'min) 0))
  (assert (equal (list-aggregate (list 1 2 3 4 6 2 100 2 3 4 0 3 4 2 3 4 5) #'max) 100))

  (assert (equal (list-unique (list 1 1 1 1 2 3 3 3 4 5 5 5 6 7 8 9 9 10 10)) (list 1 2 3 4 5 6 7 8 9 10)))

  (assert (equal (get-param (list :a 1 :b 2 :c 3 :a 4) :a) 1))
  (assert (null  (get-param (list "a" 1 "b" 2 "c" 3 "a" "david") "d")))
  (assert (equal (get-param (list "a" 1 "b" 2 "c" 3 "a" "david") "b") 2))
  (assert (equal (get-param-list (list "a" 1 "b" 2 "c" 3 "a" "david") "b") (list 2)))
  (assert (equal (get-param-list (list "a" 1 "b" 2 "c" 3 "a" "david") "a") (list 1 "david")))

)