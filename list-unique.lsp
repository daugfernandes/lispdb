;;; unique (list)
;;         Retorna lista composta pelos elementos únicos da lista ordenada L.
;;
;;  Autor: David Fernandes
;;         <daugfernandes@aim.com>
;;
;;   Data: 21-11-2010
;;
;;   Nota: O código aqui apresentado deve funcionar como ele acha que deve.
;;         Eu, o seu autor, não tenho qualquer poder sobre isso.
;;         Nessa medida, é pouco natural que este mesmo código faça aquilo
;;         que você quer. Pode acontecer, mas não é garantido.
;;
;;    Obs: É mesmo obrigatório que a lista L esteja ordenada. Como se deve
;;         perceber pela leitura do código da função.

(defun unique (L)
  (if (null L)
    nil
    (if 
       (eq (first L) (second L))
       (unique (rest L))
       (append (list (first L)) (unique (reat L)))
    )
  )
)
