;;   Copyright (C) 2010  David Fernandes
;;                       <daugfernandes@aim.com>
;;
;;   cl-nspire - TI nspire common-lisp functions
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

(defun nsp-abs (v)
  (cond 
   ((null v) nil)
   ((complexp v)
    (sqrt (+ (expt (realpart v) 2) (expt (imagpart v) 2))))  ; same as abs
   ((listp v)
    (apply-to-list v #'nsp-abs))
   ((numberp v)
    (abs v))
  )
)

(defun nsp-sign (v)
  (cond 
   ((null v) nil)
   ((listp v)
    (apply-to-list v #'nsp-sign))
   ((numberp v)
    (/ v (nsp-abs v)))
  )
)

(defun apply-to-list (v func)
  (cond
   ((null v) nil)
   (t (cons (funcall func (car v)) (apply-to-list (cdr v) func)))
  )
)

(defun nsp-angle (cplx)
  (cond
   ((null cplx) nil)
   ((listp cplx)
    (apply-to-list cplx #'nsp-angle))
   ((complexp cplx)
    (- (/ (* pi (nsp-sign (imagpart cplx))) 2)
       (atan (/ (realpart cplx)(imagpart cplx)))))
   ((numberp cplx) 0)
  )
)

(defun nsp-deg-to-rad  (v) (* (/ v 180) pi))
(defun nsp-rad-to-deg  (v) (* (/ v pi) 180))
(defun nsp-deg-to-grad (v) (* (/ v 90) 100))
(defun nsp-grad-to-deg (v) (* (/ v 100) 90))
(defun nsp-rad-to-grad (v) (* (/ v pi) 200))
(defun nsp-grad-to-rad (v) (* (/ v 200) pi))

