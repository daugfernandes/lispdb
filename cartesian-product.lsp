;;; cartesian-product (list1 list2)
;;         Retorna lista composta pelo produto cartesiano de duas listas.
;;
;;  Autor: David Fernandes
;;         <daugfernandes@aim.com>
;;
;;   Data: 29-11-2010
;;
;;   Nota: O código aqui apresentado deve funcionar com ele acha que deve.
;;         Eu, o seu autor, não tenho qualquer poder sobre isso.
;;         Nessa medida, é pouco natural que este mesmo código faça aquilo
;;         que você quer. Pode acontecer, mas não é garantido.
;;

(defun cartesian-product(L1 L2)
  (if
    (not (null L1))
    (append
      (if
        (not (null L2))
        (append 
          (list (list (first L1) (first L2)))
          (cartesian-product (list (first L1)) (rest L2))
        )
      )
      (cartesian-product (rest L1) L2)
    )
  )
)

