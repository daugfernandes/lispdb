;;;  s7-relational-algebra
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

;;  returns attributes list
(defun attributes (relation)
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
;; primitive operators

;; selection (sigma)
(defun select (relation selector-fn)
   (remove-if-not selector-fn relation)
)

(defun selector (attr value)
  #'(lambda (item) (equal (getf item attr) value))
)

;; TODO cartesian-product ()
(defun cartesian-product (relation1 relation2)
)

;; TODO set-union ()
(defun set-union (relation1 relation2)
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

;; rename (rho)
(defun rename (relation from to)
  (if (not (null relation))
    (let ((new))
      (dolist (tuple relation)
        (let 
          ((proj)) 
          (dolist (field tuple)
            (setf proj
              (append 
                proj
                (list
                  (if (equal field from)
                    to
                    field
                  )
		)
              )
            )
          )
          (setf new 
            (append 
              (list proj)
	      new
            )
          )
        )
      )
      new
    )
  )
)


;; projection (pi)
;
;  see (defun s7-db-uc1) bellow on how to build a database
;
;  example:    (projection people (list :name :age :birthdate))
;
(defun projection (relation attrs)
  (if (not (null relation))
    (let ((newtuple))
      (dolist (tuple relation)
        (let 
          ((proj 
            (cons 
              (list 
                (car attrs) 
                (getf tuple (car attrs))
              )
              nil
            )
          ))
          (dolist (attr (cdr attrs))
            (setf 
              (cdr 
	        (nthcdr 
		  (1- (length (car proj))) 
		  (car proj)
		)
	      )
              (list 
	        attr 
		(getf tuple attr)
	      )
            )
          )
          (setf newtuple 
            (append 
              proj
	      newtuple
            )
          )
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