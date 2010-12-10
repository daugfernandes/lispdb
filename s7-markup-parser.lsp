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

(defvar *lang-definition-ht* nil)

(defvar *current-state* nil
  "Function with current-state.")

(defvar *document* nil 
  "Parsed document.")

(defvar *tree* nil      
  "Document tree.")

(defvar *current-string* nil
  "Chars so far.")

(defun parse-stream (stream)
  "Parse a stream of chars."
  (let* ((*state* #'document)
	(*document* (list :document))
        (*tree* (list *document*))
	(*current-string* (make-empty-string)))
    (catch 'end-of-file
      (loop
       (funcall *state* (read-char stream nil :eof))
      )
    )
  )
  *document*
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
      (:ordered-list   #'ordered-list)
      (:unordered-list #'unordered-list)
      (:list-item      #'list-item)
      (:link           #'link)
      (:document       #'document)
      (:rgb            #'rgb)
    )
  )
)

(defun char-hexa (char)
  (or
    (and (char>= char #\0) (char<= char #\9))
    (and (char>= char #\A) (char<= char #\F))
    (and (char>= char #\a) (char<= char #\f))
  )
)

(defun rgb (char)
  (cond 
    ((eq char :eof)
      (emit-string)
      (throw 'end-of-file nil))
    ((not (char-hexa char))
      (emit-string)
    )
  )
  (add-char char)
)

(defun paragraph (char)
  (cond
    ((eq char :eof)
      (emit-string)
      (throw 'end-of-file nil))
    (t
      (add-char char))
  )
)

(defun ordered-list (char)
  (cond
    ((eq char :eof)
      (emit-string)
      (throw 'end-of-file nil))
    (t
      (setf li (list :list-item))
      ;(push-tail li (car *tree*))
      ;(push li *tree*)
      ;(setf para  (list :paragraph))
      ;(push-tail para (car *tree*))
      ;(push para *tree*)
      ;(change-state :paragraph)
    )
  )
)

(defun unordered-list (char)
  (add-char char)
)

(defun list-item (char)
  (add-char char)
)

(defun link (char)
  (add-char char)
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

(defun document (char)
  (cond
    ((eq char :eof)
      (emit-string)
      (throw 'end-of-file nil))
    ((char= char #\.)
      (emit-string))
    ((char= char #\#)
      (setf color (list :rgb)
            void1 (push-tail color (car *tree*))
            void2 (push color *tree*)
            void3 (change-state :rgb)))
    (t 
      (add-char char))
  )
)

(defun emit-string ()
  (cond
    (t
     (push-tail *current-string* (car *tree*))))

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

(defun s7m-to-pseudom (p)
  (let ((idiom-function (nth-value 0 (gethash :idiom-function pseudom))))
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

(defun s7m-to-lang (p)
  (cond 
    ((null p))
    ((not (listp  p))
     (concatenate 'string p " ")) ; necessary as there is no end-tag
    (t 
      (let 
        (
          (prefix (car (nth-value 0 (gethash (car p) *lang-definition-ht*))))
          (suffix (car (cdr (nth-value 0 (gethash (car p) *lang-definition-ht*)))))
        )
        (concatenate 'string prefix (extract p #'s7m-to-lang) suffix)
      )
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

;; Utils
(defun push-tail (element L)
  (if 
    (null (cdr L))
    (setf (cdr L) (cons  element nil))
    (push-tail element (cdr L))
  )
)

(defmacro what-if-nil (value if-true if-false)
  `(if (null ,value) ,if-true ,if-false)
)

(setf html (make-hash-table))
(setf (gethash :idiom-function html) #'s7m-to-html)
(setf (gethash :document html) (list "<body>" "</body>"))
(setf (gethash :paragraph html) (list "<p>" "</p>"))
(setf (gethash :ol html) (list "<ol>" "</ol>"))
(setf (gethash :rgb html) (list "<color>" "</color>"))
(setf (gethash :ul html) (list "<ul>" "</ul>"))
(setf (gethash :li html) (list "<li>" "</li>"))
(setf (gethash :unknown-tag html) (list "<span style=\"background-color=red;\">" "</span>"))
(setf (gethash :string-suffix html) nil)

(setf pseudom (make-hash-table))
(setf (gethash :idiom-function pseudom) #'s7m-to-pseudom)
(setf (gethash :document pseudom) (list ":document " nil))
(setf (gethash :paragraph pseudom) (list ":paragraph " nil))
(setf (gethash :ol pseudom) (list ":ordered-list " nil))
(setf (gethash :rgb pseudom)(list ":rbg " nil))
(setf (gethash :ul pseudom) (list ":unordered-list " nil))
(setf (gethash :li pseudom) (list ":list-item " nil))
(setf (gethash :unknown-tag pseudom) (list ":oops " nil))
(setf (gethash :string-suffix pseudom) " ")

(setf q (list :document (list :ol (list :li (list :paragraph "cobol")) (list :li (list :paragraph "lisp")))))
(setf p "(:DOCUMENT (:OL (:LI (:PARAGRAPH \"cobol\")) (:LI (:PARAGRAPH \"lisp\"))))")
