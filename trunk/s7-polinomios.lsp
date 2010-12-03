;;;  s7-polinomios.lsp
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

(defmacro monomio (coeficiente expoente) 
  `(list ,coeficiente ,expoente)
)

(defmacro coeficiente (monomio) 
  `(car ,monomio)
)

(defmacro expoente (monomio) 
  `(car (cdr ,monomio))
)

(defun monomios-grau (monomios)
  (car monomios)
)

(defun monomio-compara-expoente (monomio1 monomio2 func)
  (funcall func (expoente monomio1) (expoente monomio2))
)

(defun monomio-compara-coeficiente (monomio1 monomio2 func)
  (funcall func (coeficiente monomio1) (coeficiente monomio2))
)

(defun monomio-expoente-maior (monomio1 monomio2)
  (if (monomio-compara-expoente monomio1 monomio2 #'>) monomio1 monomio2)
)

(defun monomio-expoente-menor (monomio1 monomio2)
  (if (monomio-compara-expoente monomio1 monomio2 #'<) monomio1 monomio2) 
)

(defun monomio-coeficiente-maior (monomio1 monomio2)
  (if (monomio-compara-coeficiente monomio1 monomio2 #'>) monomio1 monomio2)
)

(defun monomio-coeficiente-menor (monomio1 monomio2)
  (if (monomio-compara-coeficiente monomio1 monomio2 #'<) monomio1 monomio2) 
)

(defun polinomio (monomios)
  (list 
    (monomios-simplificados monomios)
    (expoente (list-aggregate monomios #'monomio-expoente-maior))
  )
)

(defun monomios-forma-canonica (m)
  (sort (copy-seq m) #'monomio-expoente-maior)
)

(defun monomios-simplificados (m)
  (monomios-forma-canonica
    (if (null (cdr m))
      (list (car m))
      (if 
        (not (equal (expoente (car m))
                    (expoente (car (cdr m))))
        )

        (append
          (list (car m))
          (monomios-simplificados (cdr m))
        )

        (monomios-simplificados
          (append
            (list (monomio 
              (+ (coeficiente (car m)) (coeficiente (car (cdr m))))
              (expoente (car m)))
            )
            (cdr (cdr m)))
        )   
      )
    )
  )
)

