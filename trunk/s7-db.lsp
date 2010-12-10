;;;  s7-db
;;
;;   Db
;;
;;   http://en.wikipedia.org/wiki/Relational_algebra
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

(defun l ()
  (load "s7-db.lsp")
)

;; ======================================================================
;; Save/load relation to file
;
;  Based on Peter Siebel's PRATICAL COMMON LISP Chapter 2 sample code
;------------------------------------------------------------------------
(defun save-relation (relation filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print relation out)))
)

(defun load-relation (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in)))
)

;;  returns fields list
(defun fields (relation)
  (let ((aux nil) 
        (pcdr (car relation)))
    (dolist (field (car relation))
      (if (not (null (car pcdr)))
        (setf aux (append aux (list (car pcdr))))
      )
      (setf pcdr (cdr (cdr pcdr)))
    ) 
    aux
  )
)

;;=======================================================================
;                          PRIMITIVE OPERATORS
; -----------------------------------------------------------------------
; Selection (sigma)
;
; Based on Peter Siebel's PRATICAL COMMON LISP sample CDs database

;-----------------------------------------------------------------------
;
; USAGE:
;          (make-comparison-expr :name #'EQUAL "JOHN")
;       => (FUNCALL #<SYSTEM-FUNCTION EQUAL> (GETF TUPLE :NAME) "JOHN")
;
;          (make-comparison-expr :age #'< 50)
;       => (FUNCALL #<SYSTEM-FUNCTION <> (GETF TUPLE :AGE) 50)
;
(defun make-comparison-expr (field comparison value)
  "Builds comparison expression"
  `(funcall ,comparison (getf tuple ,field) ,value)
)

;-----------------------------------------------------------------------
; USAGE:
;          (make-comparisons-list (list :age #'> 40 :country #'EQUAL "USA"))
;
;       => ((FUNCALL #<SYSTEM-FUNCTION >> (GETF TUPLE :AGE) 40)
;           (FUNCALL #<SYSTEM-FUNCTION EQUAL> (GETF TUPLE :COUNTRY) "USA"))
;
(defun make-comparisons-list (fields)
  "Builds a complex comparison expressions list"
  (loop while fields
    collecting (make-comparison-expr (pop fields) (pop fields) (pop fields)))
)

;-----------------------------------------------------------------------
; USAGE:
;          (where :age #'> 40 :country #'EQUAL "USA")
;
;       => #<FUNCTION :LAMBDA (TUPLE)
;            (AND (FUNCALL #'> (GETF TUPLE :AGE) 40)
;             (FUNCALL #'EQUAL (GETF TUPLE :COUNTRY) "USA"))>
;
(defmacro where (&rest clauses)  
  "Selector function"
  `#'(lambda (tuple) (and ,@(make-comparisons-list clauses)))
)

;-----------------------------------------------------------------------
; USAGE:
;          (select costumers (where :name #'equal "MANUEL" :age #'< 50))
;
;       => ((:NAME "MANUEL" :ADDRESS "OAK ST" :CITY "CHICAGO" :AGE 43))
;
(defun select (relation selector-fn)
  "Select function based on a 'where' selector function"
  (remove-if-not selector-fn relation)
)

;; union-compatible tests if two relations have the same fields
(defun union-compatible (relation1 relation2)
  (equal                                ;compares both lists os field names
    (sort (fields relation1) #'STRING<) ;sort field names alphabeticaly
    (sort (fields relation2) #'STRING<) ;
  )
)

;; TODO cartesian-product ()
(defun cartesian-product (relation1 relation2)
)

;; TODO set-union ()

;  (setq r1 (list 
;     (list :nome "manuel" :age 33) 
;     (list :age 40 :nome "david")))
;
;  => ((:NOME "manuel" :AGE 33) 
;      (:AGE 40 :NOME "david"))
;
;  (setq r2 (list 
;     (list :nome "patricia" :idade 38 :cidade "PORTO") 
;     (list :idade 14 :nome "beatriz" :cidade "MAIA")))
;
;  => ((:NOME "patricia" :IDADE 38 :CIDADE "PORTO")
;      (:IDADE 14 :NOME "beatriz" :CIDADE "MAIA"))
;
;  NOTE that r1 and r2 are not union-compatible but with the help
;       of PROJECTION and RENAME operators:
;
;  (set-union (projection r2 (list :nome :idade)) (rename r1 :age :idade))
;
;  => ((:NOME "patricia" :IDADE 38) 
;      (:NOME "beatriz" :IDADE 14) 
;      (:NOME "david" :IDADE 40)
;      (:NOME "manuel" :IDADE 33))
(defun set-union (relation1 relation2)
  (if (union-compatible relation1 relation2)
    (unique-tuples 
      (projection (append relation1 relation2) (fields relation1))
    )
  )
)

(defun tuple-as-string (tuple)
  (format nil "~a" tuple)
)

(defun sort-ascending (tuple1 tuple2)
  (string< (tuple-as-string tuple1) (tuple-as-string tuple2))
)

(defun sort-descending (tuple1 tuple2)
  (string> (tuple-as-string tuple1) (tuple-as-string tuple2))
)

; todo
(defun unique-tuples (relation)
; sort fields
; projection
; sort tuples
; remove duplicates as they are sorted and so consecutive
  relation
)

;; TODO difference ()
(defun difference (relation1 relation2)
)

;; TODO set-intersection ()
(defun set-intersection (relation1 relation2)
)

;; TODO set-division ()
(defun set-division (relation1 relation2)
)

;; natural-join ()
(defun natural-join (relation1 relation2)
)

;-----------------------------------------------------------------------
; Rename (rho)
;-----------------------------------------------------------------------
; USAGE:
;          (rename costumers :name :newname)
;
;       => ((:NEWNAME "MANUEL" :ADDRESS "OAK ST" :CITY "CHICAGO" :AGE 43))
;
(defun rename (relation from to)
  (if (not (null relation))
    (let ((newtuple))
      (dolist (tuple relation)
        (let 
          ((proj)) 
          (dolist (field tuple)
            (setf proj
              (append 
                proj
                (list
                  (if (equal field from) to field)
		)
              )
            )
          )
          (setf newtuple 
            (append (list proj) newtuple)
          )
        )
      )
      newtuple
    )
  )
)


;-----------------------------------------------------------------------
; Projection (pi)
;-----------------------------------------------------------------------
; USAGE:
;          (projection people (list :name :age :birthdate))
;
;       => ((:NAME "MANUEL" :AGE 56 :BIRTHDATE "2011-01-05")
;           (:NAME "DAVID" :AGE 44 :BIRTHDATE "2011-11-01"))
;
(defun projection (relation fields)
  (if (not (null relation))
    (let ((newtuple))
      (dolist (tuple relation)
        (let 
          ((proj 
            (cons (list (car fields) 
                        (getf tuple (car fields)))
                   nil)
          ))
          (dolist (field (cdr fields))
            (setf 
              (cdr 
	        (nthcdr 
		  (1- (length (car proj))) 
		  (car proj)
		)
	      )
              (list field (getf tuple field))
            )
          )
          (setf newtuple (append proj newtuple))
        )
      )
      newtuple
    )
  )
)

;;======================================================================
; USE CASES
;

;(defun s7-db-uc1 ()
  (setq pilotos 
    (list
      (list :nomepiloto "MANUEL"
            :morada "RUA DE CIMA"
            :localidade "MAIA"
            :idade 43)
      (list :nomepiloto "DAVID"
            :morada "RUA DE BAIXO"
            :localidade "PORTO"
            :idade 27)
      (list :nomepiloto "JOAQUIM"
            :morada "AVENIDA DE LIBERDADE"
            :localidade "LISBOA"
            :idade 34)
      (list :nomepiloto "MARIA"
            :morada "TRAVESSA DIREITA"
            :localidade "VIESU"
            :idade 56)
    )
  )
;)

(defun s7-pilotos-por-idade ()
  (select pilotos #'(lambda (tuple) (> (getf tuple :idade) 40)))
)