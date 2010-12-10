;;;  s7-markup-parser
;;
;; ----------------------------------------------------------------------
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
;;
;; ----------------------------------------------------------------------

(in-package :common-lisp-user)
(in-package :common-lisp)
(in-package :ext)

(defpackage :s7-markup-parser
  (:use :common-lisp :ext)
  (:export  #:parse-stream #:test-stream #:extract))

(in-package :s7-markup-parser)

(defvar *current-state* nil
  "Function with current-state.")

(defvar *document*
  "Parsed document.")

(defvar *current-string* nil
  "Chars so far.")

(defun parse-stream (stream)
  "Read file to parse."
  (let ((*state* #'state0)
	(*document* (list :document))
	(*current-string* (make-empty-string)))
    (catch 'end-of-file
      (loop
       (funcall *state* (read-char stream nil :eof))
      )
    )
    *document*
  )
)

(defun test-stream (str)
  (with-input-from-string (s str)
    (parse-stream s)
  )
)

(defun change-state (state)
  (setf *state*
    (ecase state
      (:paragraph      #'paragraph)
      (:list-starting  #'list-starting)
    )
  )
)

(declaim (inline add-char))
(defun add-char (char)
  (declare  (type base-char char))
  (vector-push-extend char *current-string*))

(defun make-empty-string ()
  (make-array 0
	      :fill-pointer 0
	      :adjustable   t
	      :element-type 'base-char))

(defun state0 (char)
  (cond
    ((eq char :eof)
      (emit-string)
      (throw 'end-of-file nil))
    ((char= char #\.)
      (emit-string))
    (t 
      (add-char char))
  )
)

(defun emit-string ()
  (cond
    (t
     (push-tail *current-string* *document*)))

  (setf *current-string*  (make-empty-string))
)

(defun push-tail (element L)
  (if 
    (null (cdr L))
    (setf (cdr L) (cons  element nil))
    (push-tail element (cdr L))
  )
)

(defun s7m-to-html (p)
  (let ((idiom-function #'s7m-to-html))
    (cond 
      ((null p))
      ((not (listp  p))
       (concatenate 'string p))
      ((eq (car p) :document) 
       (concatenate 'string "<body>" 
                            (extract p idiom-function) 
                            "</body>"))
      ((eq (car p) :ol)
       (concatenate 'string "<ol>" 
                            (extract p idiom-function) 
                            "</ol>"))
      ((eq (car p) :paragraph)
       (concatenate 'string "<p>" 
                            (extract p idiom-function) 
                            "</p>"))
      ((eq (car p) :ul)
       (concatenate 'string "<ul>" 
                            (extract p idiom-function) 
                            "</ul>"))
      ((eq (car p) :li)
       (concatenate 'string "<li>" 
                            (extract p idiom-function) 
                            "</li>"))
      (t 
       (concatenate 'string "<oops>" 
                            (extract p idiom-function) 
                            "</oops>"))
    )
  )
)

(defun s7m-to-mstr (p)
  (let ((idiom-function (gethash :idiom-function mstr)))
    (cond 
      ((null p))
      ((not (listp  p))
       (concatenate 'string p " ")) ; necessary as there is no end-tag
      ((eq (car p) :document) 
       (concatenate 'string ":document " 
                             (extract p idiom-function)))
      ((eq (car p) :paragraph)
       (concatenate 'string ":paragraph " 
                             (extract p idiom-function)))
      ((eq (car p) :ol)
       (concatenate 'string ":ol " 
                             (extract p idiom-function)))
      ((eq (car p) :ul)
       (concatenate 'string ":ul " 
                             (extract p idiom-function)))
      ((eq (car p) :li)
       (concatenate 'string ":li " 
                             (extract p idiom-function)))
      (t 
       (concatenate 'string ":oops " 
                             (extract p idiom-function)))
      
    )
  )
)


(defun extract (p idiom-function)
  (let ((st ""))
    (loop for i in (cdr p) do 
      (setf st 
        (concatenate 'string st (funcall idiom-function i))
      )
    )
    st
  )
)

(setf html (make-hash-table))
(setf (gethash :idiom-function html) #'s7m-to-html)
(setf (gethash :document html) (list "<body>" "</body>"))
(setf (gethash :paragraph html) (list "<p>" "</p>"))
(setf (gethash :ol html) (list "<ol>" "</ol>"))
(setf (gethash :ul html) (list "<ul>" "</ul>"))
(setf (gethash :li html) (list "<li>" "</li>"))

(setf mstr (make-hash-table))
(setf (gethash :idiom-function mstr) #'s7m-to-mstr)
(setf (gethash :document mstr) (list ":document" nil))
(setf (gethash :paragraph mstr) (list ":paragraph" nil))
(setf (gethash :ol mstr) (list ":ordered-list" nil))
(setf (gethash :ul mstr) (list ":unordered-list" nil))
(setf (gethash :li mstr) (list ":list-item" nil))
