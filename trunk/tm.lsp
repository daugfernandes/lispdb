;;;  titm - the incredible turing machine
;;
;;   Uma máquina de turing 
;;
;;   a partir da ideia do meu amigo e colega na 
;;   Licenciatura em Informática na Universidade Aberta 
;;   António Dias
;;
;;   Copyright (c) 2011  David Fernandes
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
;;   How to:
;;   ======
;;
;;   1 - criar um ficheiro de texto com a tabela de transições
;;          (uma transição é um tuplo com 5 elementos separados por espaço:
;;             estado actual      (string)
;;             símbolo de entrada (char)
;;             estado seguinte    (string)
;;             símbolo de saída   (char)
;;             direcção da cabeça ("r" ou "l")
;;    
;;          (linhas iniciadas com ; são ignoradas e podem ser utilizadas 
;;           para inserir comentários)
;;
;;   2 - invocar a função test com os seguintes argumentos:
;;             string a testar
;;             nome do ficheiro com a tabela de transições
;;             estado inicial
;;             estado final
;;             
;; ----------------------------------------------------------------------

(defvar *delta* nil)
(defconstant head-right "r")
(defconstant head-left "l")
(defconstant blank-simbol "B")

;;======================================================================
;;                              inits
;;======================================================================
(setf *delta* (make-hash-table :test #'equalp))
;;---------------------------------------------------------------------

(defun myversion ()
  (nth-value 0 
    (format t "~%~%~%titm - the incredible turing machine~3,8@Tv.0.0.0 (pré-alpha)~%~%~%")))

(defun test (string delta-file ini-state end-state)

  (let* (

	 (tape (append                ; adds BLANKS to both ends of 
                                      ; the string to test
	        (list (character blank-simbol)) 
		(string-to-list string)
	        (list (character blank-simbol))))
	 (idx 1)                      ; head position
	 (state ini-state))           ; starting state
	 (delta-from-file delta-file) ; read a file with the transition 
                                      ; table into a hash-table
	 
	 ; Main loop

         ; `delta´ will get the 3 elements tuple of the right hand of teh transition
         ; a complete transition is a 5 elements tuple:

	 ;    left-hand  (from-state input-symbol)
         ;    right-hand (to-state output-symbol direction)

	 (loop

	   (setf delta (nth-value 0 (gethash (list state (character (nth idx tape))) *delta*)))

	   (if (null delta)
	     (return (format t "No no no! [~a] is not a good string!~%~%   Tape: [~a]~%   Head: ~a" string tape (+ 1 idx)))
                                       ; no transition under these conditions
                                       ;  -> machine dies

	     (progn                    ; transition found

	       (setf (nth idx tape)     ; updates the tape
		     (delta-output-symbol delta))

	       (setf state (delta-state-to delta)) ; updates state

	       (if 

	         (head-right-p (delta-direction delta))

		 (progn                ;move right
		   (incf idx)
		   (if 
		     (> idx (length tape))
		     (append tape (list blank-simbol))))

		 (progn                ;move left
		   (decf idx)
		   (if 
		     (< idx 0)
		     (progn
		       (append (list blank-simbol) tape)
		       (setf idx 0)))))
	        (if                     ; tests for final state
		  (equal (delta-state-to delta) end-state)
		  (return 
                     (format t "Yup! [~a] is a good string!~%~%   Tape: [~a]~%   Head: ~a" string tape (+ 1 idx)))))))))

;;======================================================================
;;                         file-read stuff
;;======================================================================
(defun delta-from-file (filename)
  "Builds the transitions table from a file."
  (setf *delta* (make-hash-table :test #'equalp))
  (let ((k 0)  ;line counter
	(in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
	 while line do
	   (parse-line line (incf k))))
    (close in)))

;;----------------------------------------------------------------------
(defun parse-line (line k)
  "Parse line and add a new transition to delta."
  (if (> (length line) 0)
      (if (not (char= (char line 0) #\;))
	  (add-transition k (transition (split line #\Space)) *delta*))))

;;======================================================================
;;                     transition-related utilities
;;======================================================================
(defmacro add-transition (k tr ta)

  "Adds transition `tr' to table `ta';
   `k' is for debug purposes indicating the line number being processed." 

  `(let ((h (gethash (transition-from ,tr) ,ta)))
     (if (not (null (cadr h)))
       (error (format t "Erro na linha [~a]: transição [~a] duplicada." ,k ,tr))
       (setf (gethash (transition-from ,tr) ,ta) (transition-to ,tr)))))

;;----------------------------------------------------------------------
(defmacro transition (l)
  "Gets a 5 elements list `L´ and convert [1] and [4] to chars."
  `(list 
     (car ,l) 
     (character (cadr ,l))
     (caddr ,l) 
     (character (cadddr ,l))
     (cadr (cdddr,l))))

;;----------------------------------------------------------------------
(defmacro transition-from (tr)
  "`From´ part of a transition: (state-from input-symbol)."
  `(list (car ,tr) (cadr ,tr)))

;;----------------------------------------------------------------------
(defmacro transition-to (tr)
  "`To´ part of a transition: (state-to output-symbol direction."
  `(cddr ,tr))

;;----------------------------------------------------------------------
(defun head-right-p (m)
  "Just two predicates for head direction tests."
  (equal m head-right))
(defun head-left-p (m)
  (equal m head-left))

;;----------------------------------------------------------------------
(defmacro delta-state-from (tr)
  "A bunch of accessors to the elements of a transition tuple."
  `(car ,tr))

(defmacro delta-state-to (tr)
  `(nth 0 ,tr))

(defmacro delta-input-symbol (tr)
  `(nth 1 ,tr))

(defmacro delta-output-symbol (tr)
  `(nth 1 ,tr))

(defmacro delta-direction (tr)
  `(nth 2 ,tr))

;;======================================================================
;;                            list-related utilities
;;======================================================================
(defun string-to-list (s)
  "Returns a list of s's chars."
  (loop for char across s collect char))
  
;;----------------------------------------------------------------------
(defmacro push-tail (element L)
  "Inserts `element' at the tail of list `L'."
  `(if (null ,L)
    (setf ,L (cons ,element nil))
    (push-tail ,element (cdr ,L))))

;;----------------------------------------------------------------------
(defmacro pop-tail (L)
  "Removes last element of list `L'".
  `(if (null (cdr ,L))
    (let ((tmp (car ,L)))
      (setf ,L nil)
      tmp)
    (pop-tail (cdr ,L))) )

;;----------------------------------------------------------------------
(defun tail (L)
  "Last element of list `L'".
  (if (null (cdr L))
    L
    (tail (cdr L))))

;;----------------------------------------------------------------------
(defun split (string separator)
  "List with all the substrings of `string' separated by `separator'."
  (setf p (position separator string :test #'equal))
  (if
    (null p)
    (list string)
    (append (list (subseq string 0 p)) (split (subseq string (+ 1 p)) separator))))

;;======================================================================

(myversion)


  