;;;  titm - the incredible turing machine
;;
;;   Uma mÃ¡quina de turing 
;;
;;   a partir da ideia do meu amigo e colega na 
;;   Licenciatura em InformÃ¡tica na Universidade Aberta 
;;   AntÃ³nio Dias
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
;;   1 - criar um ficheiro de texto com a definiÃ§Ã£o da mÃ¡quina e tabela de transiÃ§Ãµes
;;
;;          linha de definiÃ§Ã£o da mÃ¡quina:
;;
;;          # estado-inicial simbolo-blank estado-final [estado-final-n ...] 
;;          
;;          linha de transiÃ§Ã£o
;;          (uma transiÃ§Ã£o Ã© um tuplo com 5 elementos separados por espaÃ§o:
;;             estado actual      (string)
;;             sÃ­mbolo de entrada (char)
;;             estado seguinte    (string)
;;             sÃ­mbolo de saÃ­da   (char)
;;             direcÃ§Ã£o da cabeÃ§a ("r" ou "l")
;;    
;;          (linhas iniciadas com ; sÃ£o ignoradas e podem ser utilizadas 
;;           para inserir comentÃ¡rios)
;;
;;   2 - invocar a funÃ§Ã£o test com os seguintes argumentos:
;;             string a testar
;;             nome do ficheiro com a tabela de transiÃ§Ãµes
;;             estado inicial
;;             estado final
;;             
;; ----------------------------------------------------------------------

(defvar *delta* nil)
(defvar *start-state* nil)
(defvar *head-moves* nil)

(defvar *end-states* nil)
(defvar *cur-state* nil)
(defvar *blank-symbol* nil)

(defconstant head-right "r")
(defconstant head-left "l")

;;======================================================================
;;                              inits
;;======================================================================
(setf *delta* (make-hash-table :test #'equalp))
;;---------------------------------------------------------------------

(defun myversion ()
  (nth-value 0 
    (format t "~%~%~%titm - the incredible turing machine~3,8@Tv.0.0.1~%~%~%")))

(defun test (string delta-file)

  (let* ((idx 1))                         ; head position

    (setf *delta* (make-hash-table :test #'equalp)
	  *previous-states* (make-hash-table :test #'equalp)
	  *head-moves* 0
	  *cur-state* (tm-from-file delta-file)
		 			  ; read a file with the rm definition
					  ; and sets startstate
	  tape (append                    ; adds BLANKS to both ends of 
					  ; the string to test
		(list (character *blank-symbol*)) 
		(string-to-list string)
		(list (character *blank-symbol*))))
	 
    
    ; Main loop

    ; `deltaÂ´ will get the 3 elements tuple of the right hand of the transition
    ; a complete transition is a 5 elements tuple:
    ;    left-hand  (from-state input-symbol)
    ;    right-hand (to-state output-symbol direction)

    (loop
     
     (setf delta (nth-value 0 (gethash (list *cur-state* (character (nth idx tape))) *delta*)))
     
     (if (null delta)
	 (return (format t "No no no! [~a] is not a good string!~%~%   Tape: [~a]~%   Head: ~a" string tape (+ 1 idx)))
 					  ; no transition under these conditions
					  ;  -> machine dies
       
       (progn                             ; transition found

	 (setf (nth idx tape)             ; updates the tape
	       (delta-output-symbol delta))
	 
	 (setf *cur-state* (delta-state-to delta)) ; updates state

	 (incf *head-moves*)

	 (if 
	     
	     (head-right-p (delta-direction delta))
	     
	     (progn                       ;move right
	       (incf idx)
	       (if 
		   (> idx (length tape))
		   (append tape (list *blank-symbol*))))
	   
	     (progn                       ;move left
	       (decf idx)
	       (if (< idx 0)
		   (progn
		     (append (list *blank-symbol*) tape)
		     (setf idx 0)))))

	 (if                              ; tests for final states
	     (has *end-states* (delta-state-to delta) #'equal)
	     (return 
	      (format t "Yup! [~a] is a good string!~%~%   Tape: [~a]~%   Head: [~a]~%   Final state: [~a]~%   Head moves: [~a]~%~%" string tape (+ 1 idx) (delta-state-to delta) *head-moves*))))))))

;;======================================================================
;;                         file-read stuff
;;======================================================================
(defun tm-from-file (filename)
  "Builds the transitions table from a file."
  (let ((k 0)  ;line counter
	(in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
	 while line do
	   (parse-line line (incf k))))
    (close in))
  *start-state*)

;;----------------------------------------------------------------------
(defun parse-line (line k)
  "Parse line and add a new transition to delta."
  (let ((sline (split line #\Space)))
    (if (> (length line) 0)
	(if (not (char= (char line 0) #\;))
	    (if (char= (char line 0) #\#)
		(progn
		    (setf *start-state* (cadr sline))
		    (setf *blank-symbol* (caddr sline))
		    (setf *end-states* (cdddr sline)))
	        (add-transition k (transition sline) *delta*))))))

;;======================================================================
;;                     transition-related utilities
;;======================================================================
(defmacro add-transition (k tr ta)

  "Adds transition `tr' to table `ta';
   `k' is for debug purposes indicating the line number being processed." 

  `(let ((h (gethash (transition-from ,tr) ,ta)))
     (if (not (null (cadr h)))
       (error (format t "Erro na linha [~a]: transiÃ§Ã£o [~a] duplicada." ,k ,tr))
       (setf (gethash (transition-from ,tr) ,ta) (transition-to ,tr)))))

;;----------------------------------------------------------------------
(defmacro transition (l)
  "Gets a 5 elements list `LÂ´ and convert [1] and [4] to chars."
  `(list 
     (car ,l) 
     (character (cadr ,l))
     (caddr ,l) 
     (character (cadddr ,l))
     (cadr (cdddr,l))))

;;----------------------------------------------------------------------
(defmacro transition-from (tr)
  "`FromÂ´ part of a transition: (state-from input-symbol)."
  `(list (car ,tr) (cadr ,tr)))

;;----------------------------------------------------------------------
(defmacro transition-to (tr)
  "`ToÂ´ part of a transition: (state-to output-symbol direction."
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

;;----------------------------------------------------------------------
(defun has (L element eqf)
  "Search `LÂ´ for `elementÂ´."
  (if (null L)
      nil
      (if
	  (funcall eqf element (car L))
	  t
	  (has (cdr L) element eqf))))
	  
;;======================================================================
;;                            string-related utilities
;;======================================================================
(defun replicate (s n)
  "Replicates string `sÂ´ `nÂ´times."
  (if (<= n 0)
      ""
      (concatenate 'string s (replicate s (- n 1))))) 

;;======================================================================

(myversion)
