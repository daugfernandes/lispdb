; com duas pequenas adapta��es a coisa fica mais aberta
(defun tr (L funcao)  
  (if (null L)
    nil
    (append 
      (list (funcall funcao L))
      (tr (cdr L) funcao)
    )
  )
)

; dobro
(tr (list 1 2 3 4 5) (lambda (x) (* (car x) 2)))
; triplo
(tr (list 1 2 3 4 5) (lambda (x) (* (car x) 3)))
; a ra�z quadrada
(tr (list 1 2 3 4 5) (lambda (x) (sqrt (car x))))

;; as seguintes, como tratam elementos consecutivos, no �ltimo, retorna ele
;; pr�prio. como exemplo serve.

; a m�dia dos elementos dois a dois (sendo que o ultimo � ele pr�prio :( )
(tr (list 1 2 3 4 5) (lambda (x) (if (null (car (cdr x))) (car x) (/ (+ (car x) (car (cdr x))) 2))))
; o mmc entre dois elementos consecutivos (sendo que o ultimo � ele pr�prio :( )
(tr (list 2 4 12 4) (lambda (x) (if (null (car (cdr x))) (car x) (lcm (car x) (car (cdr x))))))
; o menor de cada dois elementos consecutivos (sendo que o ultimo � ele pr�prio :( )
(tr (list 1 5 3 12 24 48 9) (lambda (x) (if (null (car (cdr x))) (car x) (if (< (car x) (car (cdr x))) (car x) (car (cdr x))))))
