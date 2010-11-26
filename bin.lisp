(in-package :common-lisp-user)

(defpackage :se7ening-numbers
  (:use :common-lisp)
  (:export  #:bin-to-decimal
	    #:octal-to-decimal
	    #:hex-to-decimal))

(in-package :se7ening-numbers)


(defun base-n-to-decimal (lst base)
"Retorna o valor decimal de um suposto n�mero em base 'base' bin�rio representado 
 por uma lista de elementos nessa base.

  Exemplo base 2:  

         (list 1 0 1 0 0 1 1)
          length => 7
        +---+---+---+---+---+---+---+
        ! 1 ! 0 ! 1 ! 0 ! 0 ! 1 ! 1 !
        +---+---+---+---+---+---+---+
Posi��o   6   5   4   3   2   1   0

          1 * 2 ^ 6 +

         (list 0 1 0 0 1 1)
          length => 6
        +---+---+---+---+---+---+
        ! 0 ! 1 ! 0 ! 0 ! 1 ! 1 !
        +---+---+---+---+---+---+
Posi��o   5   4   3   2   1   0

          0 * 2 ^ 5 +

         (list 1 0 0 1 1)
          length => 5
        +---+---+---+---+---+
        ! 1 ! 0 ! 0 ! 1 ! 1 !
        +---+---+---+---+---+
Posi��o   4   3   2   1   0

          1 * 2 ^ 4 +

         (list 0 0 1 1)
          length => 4
        +---+---+---+---+
        ! 0 ! 0 ! 1 ! 1 !
        +---+---+---+---+
Posi��o   3   2   1   0

          0 * 2 ^ 3 +

         (list 0 1 1)
          length => 3
        +---+---+---+
        ! 0 ! 1 ! 1 !
        +---+---+---+
Posi��o   2   1   0

          0 * 2 ^ 2 +

         (list 1 1)
          length => 2
        +---+---+
        ! 1 ! 1 !
        +---+---+
Posi��o   1   0

          1 * 2 ^ 1 +

         (list 1)
          length => 1
        +---+
        ! 1 !
        +---+
Posi��o   0

          1 * 2 ^ 0 +

         (nil)
          length => 

          0
"
  (if (null lst)
    0                               ; lista vazia, vale 0
    (+                              ; lista n�o vazia, vai somar
      (*                            ; o produto
        (car lst)                     ; do d�gito representado pelo 1� elem da lista
        (expt                         ; pelo seu peso absoluto base^posi��o
          base 
          (- (length lst) 1)          ; (a posi��o � obtida atrav�s do tamanho da lista - 1)
        )
      )
      (base-n-to-decimal (cdr lst) base)
                                    ; ao valor decimal do lista composta pelos restantes
                                    ; d�gitos
    )
  )
)

(defun bin-to-decimal (lst) 
"Converte bin�rio para decimal."
  (base-n-to-decimal lst 2)
)

(defun octal-to-decimal (lst) 
"Converte octal para decimal."
  (base-n-to-decimal lst 8)
)

(defun hex-to-decimal (lst) 
"Converte hexadecimal para decimal."
  (base-n-to-decimal lst 16)
)
