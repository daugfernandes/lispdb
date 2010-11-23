;; cria a estrutura do tipo monoid
(defstruct monoid coefficient exponent)

;; funcoes de comparacao de dois monoids

; grau ascendente
(defun cmpmonoid-exponent-asc     (a b) (< (monoid-exponent a) (monoid-exponent b)))

; coeficiente ascendente
(defun cmpmonoid-coefficient-asc  (a b) (< (monoid-coefficient a) (monoid-coefficient b)))

; grau descendente
(defun cmpmonoid-exponent-desc    (a b) (> (monoid-exponent a) (monoid-exponent b)))

; coeficiente descendente
(defun cmpmonoid-coefficient-desc (a b) (> (monoid-coefficient a) (monoid-coefficient b)))

;; cria array a
(setq a (make-array 0 :element-type 'monoid))

;; ajusta tamanho do array
;(setq a (adjust-array a 2))

;; incrementa dimensao do array
(setq a (adjust-array a (+ 1 (length a))))
(setq a (adjust-array a (+ 1 (length a))))
(setq a (adjust-array a (+ 1 (length a))))
(setq a (adjust-array a (+ 1 (length a))))

;; coloca um monoid novo no array
(setf (aref a 0) (make-monoid :coefficient 1 :exponent 2))
(setf (aref a 1) (make-monoid :coefficient 4 :exponent 5))
(setf (aref a 2) (make-monoid :coefficient 2 :exponent 10))
(setf (aref a 3) (make-monoid :coefficient 7 :exponent 3))

;; funcao que retorna a propriedade c do monoid i do array a
(defun test (a i) (monoid-c (aref a i)))


(defun grau-min (a)

;; generic array element remove function
(defun array-remove (array i)
  (if i            ; i not nil
    (if (>= i 0)   ; i positive
      (let ((result (copy-array array)))  ; copy array
        (loop for j from i to (- (length result) 2) do
          (rotatef (aref result j) (aref result (+ 1 j)))
        )
        (if (< i (- (length array) 1))
          (setq result (adjust-array result (- (length result) 1)))
          array
        )
        result
      )
      array
    )
    array
  )
)

(defun copy-array (array)
 (let ((dims (array-dimensions array)))
   (adjust-array
    (make-array dims :displaced-to array)
    dims)))


;; simetrico
(defun minus (polinomio)
  (let ((result (copy-array polinomio)))  ; copy array
    (loop for j from 0 to (- (length result) 1) do
      (setf (monoid-coefficient (aref result j)) (* -1 (monoid-coefficient (aref result j))))
    )
    result
  )
)
  

;; generic bubble-sort function
;; from http://en.literateprograms.org/Bubble_sort_(Lisp)
(defun bubble-sort (array cmp-fun) 
  "Bubble sort implementation in common lisp. Using the extended loop facility."
  (let ((result (copy-array array)))
    (loop for i from (1- (length result)) downto 0 do
        (loop for j from 0 to i
            when (funcall cmp-fun (aref result i) (aref result j))
               do (rotatef (aref result i) (aref result j))
        )
    )
    result
  )
)




;; chamada de sort com um array de monoids
; ordenando por coeficiente
(bubble-sort a #'cmpmonoid-coefficient-desc)
; ordenando por grau
(bubble-sort a #'cmpmonoid-exponent-desc)


;; a) canonica(p1) - coloca o polinomio p1 na forma canonica
(defun canonica (polinomio) 
   (bubble-sort polinomio #'cmpmonoid-exponent-desc)
)

;; b) soma-polinomios(p1, p2)