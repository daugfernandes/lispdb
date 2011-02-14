;;;  tm
;;
;;   a partir da ideia, em python, do meu amigo e 
;;   colega na Licenciatura em informática na Universidade Aberta 
;;   António Dias
;;
;;
;;   Copyright (C) 2010  David Fernandes
;;                       <daugfernandes@aim.com>
;;
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
;; ----------------------------------------------------------------------

(defvar *delta* nil)
(defconstant right 'r)
(defconstant left 'l)

(defun test (string delta-file ini-state end-state blank-simbol)
  (let* ((str (string-to-list string))
	 (state ini-state)
	 (head str)
	 (*delta* (delta-from-file delta-file))

    (loop
       (setf res (car (gethash (list state (car head))) *delta*))
       (if (null res) 
	   (return)
;; todo

    )  

    (gethash (list state (first str)) *delta*)))))
    

;;---------------------------------------------------------------------
;;                         file-read stuff
;;---------------------------------------------------------------------
(defun delta-from-file (filename)
  "Builds the transitions table from a file."
  (setf *delta* (make-hash-table :test #'equalp))
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
	 while line do
	   (parse-line line)))
    (close in)))

(defun parse-line (line)
  "Parse line and add a new transition to delta."
  (add-transition (split line #\Space) *delta*))

;;----------------------------------------------------------------------
;;                     transition-related utilities
;;----------------------------------------------------------------------
(defun add-transition (tr ta)
  "Adds transition `tr' to table `ta'."
  (setf (gethash (transition-from tr) ta) (transition-to tr)))

(defmacro transition-from (tr)
  "From part of a transition: (state-from input-symbol)."
  `(list (car ,tr) (cadr ,tr)))

(defmacro transition-to (tr)
  "To part of a transition: (state-to ribbon-symbol direction."
  `(cddr ,tr))

;;----------------------------------------------------------------------
;;                            list-related utilities
;;----------------------------------------------------------------------
(defun string-to-list (s)
  "Returns a list of s's chars."
  (loop for char across s
        collect char))

(defmacro push-tail (element L)
  "Inserts `element' at the tail of list `L'."
  `(if (null ,L)
    (setf ,L (cons ,element nil))
    (push-tail ,element (cdr ,L))))

(defmacro pop-tail (L)
  "Removes last element of list `L'".
  `(if (null (cdr ,L))
    (let ((tmp (car ,L)))
      (setf ,L nil)
      tmp)
    (pop-tail (cdr ,L))) )

(defun tail (L)
  "Last element of list `L'".
  (if (null (cdr L))
    L
    (tail (cdr L))))

(defun split (string separator)
  "List with all the substrings of `string' separated by `separator'."
  (setf p (position separator string :test #'equal))
  (if
    (null p)
    (list string)
    (append (list (subseq string 0 p)) (split (subseq string (+ 1 p)) separator))))

;;----------------------------------------------------------------------
;;                            etc-related utilities
;;----------------------------------------------------------------------

  