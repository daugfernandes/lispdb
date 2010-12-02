;;;  s7-polynomials.lsp
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

(defun monomio (coeficiente expoente)
  (list "coeficiente" coeficiente "expoente" expoente)
)

(defun monomio-expoente-maior (m1 m2)
  (if (monomio-compara-expoente m1 m2 #'>) m1 m2)
)

(defun monomio-expoente-menor (m1 m2)
  (if (monomio-compara-expoente m1 m2 #'<) m1 m2) 
)

(defun monomio-coeficiente-maior (m1 m2)
  (if (monomio-compara-coeficiente m1 m2 #'>) m1 m2)
)

(defun monomio-coeficiente-menor (m1 m2)
  (if (monomio-compara-coeficiente m1 m2 #'<) m1 m2) 
)

(defun monomio-compara-expoente (m1 m2 func)
  (funcall func (get-param m1 "expoente") (get-param m2 "expoente"))
)

(defun monomio-compara-coeficiente (m1 m2 func)
  (funcall func (get-param m1 "coeficiente") (get-param m2 "coeficiente"))
)

(defun monomio-valor (m x)
  (* (get-param m "coeficiente") (expt x (get-param m "expoente")))
)

(defun monomio-simetrico (m)
  (monomio (- (get-param m "coeficiente")) (get-param m "expoente"))
)

(defun monomio-inverso (m)
  (monomio (get-param m "coeficiente") (- (get-param m "expoente")))
)

(defun monomios-simplificados (m)
  (monomios-forma-canonica
    (if (null (cdr m))
      (list (car m))
      (if 
        (not (equal (get-param (car m) "expoente")
                      (get-param (car (cdr m)) "expoente"))
        )

        (append
          (list (car m))
          (monomios-simplificados (cdr m))
        )

        (monomios-simplificados
          (append
            (list (monomio 
              (+ (get-param (car m) "coeficiente") (get-param (car (cdr m)) "coeficiente"))
              (get-param (car m) "expoente"))
            )
            (cdr (cdr m)))
        )   
      )
    )
  )
)

(defun polinomio-valor (p x)
  (if (null (get-param p "monomios"))
    0
    (+ (monomio-valor (car (get-param p "monomios")) x)
       (polinomio-valor (polinomio (cdr (get-param p "monomios"))) x))
  )
)

(defun polinomio (monomios)
  (list 
    "monomios" (monomios-simplificados monomios)
    "grau" (get-param (list-aggregate monomios #'monomio-expoente-maior) "expoente")
  )
)

(defun monomios-forma-canonica (m)
  (sort (copy-seq m) #'monomio-expoente-maior)
)

(defun polinomio-simetrico (p)
  (let ((return-value '()))
    (dolist (m (get-param p "monomios")) (push (monomio-simetrico m) return-value))
      (polinomio return-value)
  )
)

(defun polinomio-grau (p)
  (get-param p "grau")  
)

