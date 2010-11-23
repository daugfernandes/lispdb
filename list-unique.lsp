;;; unique (list)
;;         Retorna lista composta pelos elementos únicos da lista ordenada L.
;;
;;  Autor: David Fernandes
;;         <daugfernandes@aim.com>
;;
;;   Data: 21-11-2010
;;
;;   Nota: O código aqui apresentado deve funcionar com ele acha que deve.
;;         Eu, o seu autor, não tenho qualquer poder sobre isso.
;;         Nessa medida, é pouco natural que este mesmo código faça aquilo
;;         que você quer. Pode acontecer, mas não é garantido.
;;
;;    Obs: É mesmo obrigatório que a lista L esteja ordenada. Como se deve
;;         perceber pela leitura do código da função.

(defun unique (L)

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

            (unique (cdr L))    ; invocamos a mesma função mas apenas para os 
                                 ; restantes elementos esquecendo o primeiro

            ; não, a condição é falsa: o primeiro elemento é diferente do 
            ; segundo pelo que não se pode perder.

            ; utilizamos a função append para unir este primeiro elemento
            ; à lista composta pelos elementos únicos da lista a seguir a este

            (append

              (list (car L))     ; a lista constituida pelo primeiro elemento 
              (unique (cdr L))  ; a lista composta pelos elementos únicos 
                                 ; da lista composta pelos restantes elementos
        )
    )
  )
)
