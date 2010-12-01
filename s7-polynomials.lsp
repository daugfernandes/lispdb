;;;  s7-polynomials.lsp
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

(defun monomial (coefficient exponent)
  (list :c coefficient :e exponent)
)

(defun monomial-greater-exponent (m1 m2)
  (if (monomial-compare-exponent m1 m2 #'>) m1 m2)
)

(defun monomial-smaller-exponent (m1 m2)
  (if (monomial-compare-exponent m1 m2 #'<) m1 m2) 
)

(defun monomial-greater-coefficient (m1 m2)
  (if (monomial-compare-coefficient m1 m2 #'>) m1 m2)
)

(defun monomial-smaller-coefficient (m1 m2)
  (if (monomial-compare-coefficient m1 m2 #'<) m1 m2) 
)

(defun monomial-compare-exponent (m1 m2 func)
  (funcall func (getf m1 :e) (getf m2 :e))
)

(defun monomial-compare-coefficient (m1 m2 func)
  (funcall func (getf m1 :c) (getf m2 :c))
)

(defun monomial-value (m x)
  (* (getf m :c) (expt x (getf m :e)))
)

(defun monomial-symmetric (m)
  (monomial (- (getf m :c)) (getf m :e))
)

(defun monomial-inverse (m)
  (monomial (getf m :c) (- (getf m :e)))
)

(defun monomials-simplify (m)
  (monomials-canonical
    (if (null (cdr m))
      (list (car m))
      (if 
        (not (equal (getf (car m) :e)
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
            (cdr (cdr m)))
        )   
      )
    )
  )
)

(defun polynomial-value (p x)
  (if (null (getf p :monomials))
    0
    (+ (monomial-value (car (getf p :monomials)) x)
       (polynomial-value (polynomial (cdr (getf p :monomials))) x))
  )
)

(defun polynomial (monomials)
  (list 
    :monomials (monomials-canonical (monomials-group monomials))
    :degree (getf (aggregate monomials #'monomial-greater-exponent) :e)
  )
)

(defun monomials-canonical (m)
  (sort m #'monomial-smaller-exponent)
)

(defun polynomial-symmetric (p)
  (let ((return-value '()))
    (dolist (m (getf p :monomials)) (push (monomial-symmetric m) return-value))
      (polynomial return-value)
  )
)

(defun polynomial-degree (p)
  (getf p :degree)  
)

