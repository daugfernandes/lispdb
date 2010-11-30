;;;  polynomial
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

(defun list-first-n (L n)
  (if (and L (> n 0))
      (cons (car L) (list-first-n (cdr L) (1- n)))
  )
)

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

(defun monomial (coefficient exponent)
  (list :c coefficient :e exponent)
)

(defun monomial-compare-coefficient-asc (m1 m2)
  (if (> (getf m1 :c) (getf m2 :c))
    m1
    m2
  )
)

(defun monomial-compare-coefficient-desc (m1 m2)
  (if (< (getf m1 :c) (getf m2 :c))
    m1
    m2
  )
)

(defun monomial-compare-coefficient-asc (m1 m2)
  (if (> (getf m1 :c) (getf m2 :c))
    m1
    m2
  )
)

(defun monomial-compare-exponent-desc (m1 m2)
  (if (< (getf m1 :e) (getf m2 :e))
    m1
    m2
  )
)

(defun monomial-compare-exponent-asc (m1 m2)
  (if (> (getf m1 :e) (getf m2 :e))
    m1
    m2
  )
)

(defun monomial-value (m x)
  (* (getf m :c) (expt x (getf m :e)))
)

(defun monomials-group (m)
  (if 
    (null (cdr m))
    (list (car m))
    (if
      (not (equal
        (getf (car m) :e)
        (getf (car (cdr m)) :e))
      )
      (append
        (list (car m))
        (monomials-group (cdr m))
      )
      (monomials-group
        (append
          (list (monomial 
            (+ (getf (car m) :c) (getf (car (cdr m)) :c))
            (getf (car m) :e))
          )
          (cdr (cdr m))
        )
      )   
    )
  )
)

(defun polynomial-value (p x)
  (if (null (getf p :monomials))
    0
    (+
      (monomial-value (car (getf p :monomials)) x)
      (polynomial-value (polynomial (cdr (getf p :monomials))) x)
    )
  )
)

(defun aggregate (L select-function)
  (if (null (cdr L))
    (car L)
    (funcall select-function (car L) (aggregate (list-except-first-n L 1) select-function))
  )
)

(defun polynomial (monomials)
  (list 
    :monomials monomials 
    :degree (getf (aggregate monomials #'monomial-compare-exponent-asc) :e)
  )
)

(defun polynomial-degree (p)
  (getf p :degree)  
)

(defun polynomial-add-monomial(p m))
