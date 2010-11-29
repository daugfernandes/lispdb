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

;;; =======================================================================
;;  Monomial ==============================================================
(defstruct monomial 
  coefficient
  exponent
)

;;  - build macro
(defmacro monomial (_coefficient _exponent)
  `(make-monomial :coefficient ,_coefficient :exponent ,_exponent)
)

(defun monomial-symmetric (m)
   (monomial (- (monomial-coefficient m)) (monomial-exponent m))
)

;;; ========================================================================
;;; Polynomial  ============================================================
(defstruct polynomial 
  monomials
  degree
)

;;  - build macro
(defmacro polynomial (_monomials)
"Builds a polynomial from a list of monomials and produce a canonical representation, e.g, monomials sorted by exponente descending."
  `(make-polynomial 
    :monomials 
      (sort                      ; sorting monomios exponent descendant
        (copy-list ,_monomials)  ; sort is destructive so make a copy first
        #'monomial-exponent-desc ; sorting function to use
      )
    :degree     
      (aggregate             ; aggregate function
        ,_monomials          ; should return 
        #'max                ; the greatestls
        #'monomial-exponent  ; monomial exponent
      )        
   )
)

(defun polynomial-symmetric (p)
  (let ((return-value '()))
    (dolist (m (polynomial-monomials p)) (push (monomial-symmetric m) return-value))
      (polynomial return-value)
  )
)

(defun polynomial-redux (a)
"Adds same exponent monomials."
;; TODO: implement (just a test sorting inversly
  (if (not (polynomial-p a))
    nil
    (let ((q (polynomial (append (polynomial-monomials a) ()))))
      (let ((ret ()))
        (loop for m on (polynomial-monomials q)
          do (cons m 'ret)
        )
      )
    )
  )
)

(defun unique (L what-from-element)
  (if (null (car L))
    nil
    (if (eq (funcall what-from-element (car L)) (funcall what-from-element (car (cdr L))))
      (unique (cdr L) what-from-element)
      (append (list (car L)) (unique (cdr L) what-from-element))
    )
  )
)


(defun unique2 (L)

    (if (null (car L))           ; o primeiro elemento da lista L é nil??
                                 ; isto é: a lista L é uma lista vazia??

        ; sim a lista L é vazia, retorna nil porque não há elementos únicos
        ; a procurar numa lista vazia

        nil

        ; não, a lista contém elementos

        ; vejamos se o primeiro elemento é igual ao seguinte!!
        (if 
            (eq                  ;
                (car L)          ; primeiro elemento da lista
                                 ; 
                (car (cdr L))    ; elemento seguinte ao primeiro
                                 ; que não é mais nem menos que
                                 ; o primeiro elemento da lista composta
                                 ; pelos elementos da L a partir do primeiro
            )                    ;

            ; sim, a condição é verdadeira: o primeiro e o segundo elementos
            ; são iguais

	    ; assim sendo, podemos descartar o primeiro já que tem 
            ; um representante capaz no segundo :))

            (unique2 (cdr L))    ; invocamos a mesma função mas apenas para os 
                                 ; restantes elementos esquecendo o primeiro

            ; não, a condição é falsa: o primeiro elemento é diferente do 
            ; segundo pelo que não se pode perder.

            ; utilizamos a função append para unir este primeiro elemento
            ; à lista composta pelos elementos únicos da lista a seguir a este

            (append

              (list (car L))     ; a lista constituida pelo primeiro elemento 
              (unique2 (cdr L))  ; a lista composta pelos elementos únicos 
                                 ; da lista composta pelos restantes elementos
        )
    )
  )
)



;;; Monomials compare functions
;;  exponent ascending
(defun monomial-exponent-asc (a b) 
  (< (monomial-exponent a) (monomial-exponent b))
)

;; coefficient ascending
(defun monomial-coefficient-asc (a b) 
  (< (monomial-coefficient a) (monomial-coefficient b))
)

;; exponent descending or equal
(defun monomial-exponent-desc (a b) 
  (not (monomial-exponent-asc a b))
)

;; coefficient descending or equal
(defun monomial-coefficient-desc (a b) 
  (not (monomial-coefficient-asc a b))
)

;; unit-tests
(defun ut ()
  (ut-monomials)
  (ut-polynomials)
)

(defun ut-monomials ()
  (ut-monomials-build)
  (ut-monomials-compare)
)

(defun ut-polynomials ()
  (ut-polynomials)
)

(defun ut-monomials-build ()
    (assert (monomial nil nil))
    (assert (monomial 1 nil))
    (assert (monomial nil 2))
    (assert (monomial 2 3))
)

(defun ut-monomials-compare ()
    (assert (not (monomial-exponent-asc     (monomial 1 7) (monomial 2 4))))
    (assert (     monomial-exponent-desc    (monomial 1 7) (monomial 2 4)))
    (assert (     monomial-coefficient-asc  (monomial 1 7) (monomial 2 4)))
    (assert (not (monomial-coefficient-desc (monomial 1 7) (monomial 2 4))))
    (assert (     monomial-exponent-asc     (monomial -1 -7) (monomial -2 -4)))
    (assert (not (monomial-exponent-desc    (monomial -1 -7) (monomial -2 -4))))
    (assert (not (monomial-coefficient-asc  (monomial -1 -7) (monomial -2 -4))))
    (assert (     monomial-coefficient-desc (monomial -1 -7) (monomial -2 -4)))
)

(defun ut-polynomials ()
    (assert (polynomial (list (monomial 1 2)
                              (monomial 2 4)
                              (monomial 0 0)
                              (monomial 0 1)
                              (monomial 1 0)
                        )
            )
    )
)

;;; =========================================================================
;;  Utils
(defun myself (a) a)

(defun string-compare (a b func)
  (if (funcall func a b) a b)
)

(defun string-min (a b) (string-compare a b #'string<))
(defun string-max (a b) (string-compare a b #'string>))

;;; Aggregate function
(defun aggregate (L aggregate-function what-from-element)
"                 L: list

 aggregate-function: e.g. MAX, MIN; or whatever function that select one 
                     of two elements

  what-from-element: if primitive elements, themselves; if a struct you may
                     what to compare CUSTOMER-SALES (property SALES from 
                     CUSTOMER struct."
  (if (null (car L))
    nil
    (if (null (car (cdr L)))              ; car is the last element
      (funcall what-from-element (car L)) ; so get whatever from it
      (funcall aggregate-function         ; or else recursively call
               (funcall what-from-element (car L)) 
               (aggregate (cdr L) aggregate-function what-from-element)
      )
    )
  )
)
