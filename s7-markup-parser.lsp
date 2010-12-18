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


(defvar *lang-definition-ht* nil
  "Language definition hash-table for backend translator.")

(defvar *document* nil 
  "Parsed document root node.")

(defvar *tree* nil      
  "Document pushdown tree. (car *tree*) is the last element in use.")

(defvar *states* nil
  "Pushdown list os states so far.")

(defvar *header-count* 0
  "Counts *")

(defvar *current-string* nil
  "Characters processed so far.")

(defconstant *escaped-chars* "\\{}*"
  "Characters that need special prefixing.")
(defconstant *eol* #\.
  "End-of-line character.")
(defconstant *eof* :eof
  "End-of-file indicator.")

(defconstant *void-chars* (string #\Return)
  "Characters that should be ignored.")

(defun is-escaped-char (c)
  (not (null (search (string c) *escaped-chars*)))
)

(defun parse-stream (stream)
  "Parse a stream of characters."
  (setf *document* (list :document)
        *tree* (list *document*)
        *current-string* (make-empty-string))
  (push #'document *states*)
  (make-node (list :body))
  (push #'body *states*)

  (catch 'end-of-file
    (loop
      (let ((c (read-char stream nil *eof*)))
        (if (null (search (string c) *void-chars*))
          (funcall (car *states*) c)
        )
      )
    )
  )
  *document*
)

(defun test-string (str)
  "Aux function to parse a string."
  (with-input-from-string (s str)
    (parse-stream s)
  )
)

(defun test-file (filename)
  "Aux function to parse an entire text file."
  (with-open-file (stream filename :direction :input)
    (parse-stream stream)
  )
)

;; states

(defun body (c)
  "Body state. Nothing interesting happened, yet."
  (cond

    ((test-eof c))

    ((char= c \*)
     (incf *header-count*)
     (push #'header0 *states*))
  )
)

(defun header0 (char)
  "Header state *'s running."
  (cond
    ((test-eof char))
    ((char= c \*)
     (incf *header-count*))
    ((char= c #\Space)
     (make-node (list :header *header-count*))
     (push #'text *states*)
    (t
     (make-node (list :text))
     (push #'text *states*)
     (add-char char))
  )
)

(defun text (c)
  (add-char c))

(defun make-node (content)
  "Creates a new node." 
  (setf node content)
  (push-tail node (car *tree*)) ;sets it in the document structure
  (push node *tree*)            ;and in the document pushdown tree
)

(defun char-hexa (c)
  "Tests for a hexa char."
  (or
    (and (char>= c #\0) (char<= c #\9))
    (and (char>= c #\A) (char<= c #\F))
    (and (char>= c #\a) (char<= c #\f))
  )
)

(defun test-eof (c)
  (cond 
    ((char= c *eof*)
      (use-chars-read)
      (pop *tree*)
      (pop *states*)
      (change-state (car *states*))
      (throw 'end-of-file nil)
    )
  )
)

(defun add-char (c)
  (vector-push-extend c *current-string*))

(defun make-empty-string ()
  (make-array 0
	      :fill-pointer 0
	      :adjustable   t
	      :element-type 'base-char))


(defun use-chars-read ()
  "Gives effective use to the characters read so far."
  (cond
    ((> (length *current-string*) 0)
      (push-tail 
        (string-right-trim " " *current-string*)
        (car *tree*)
      )
      (setf *current-string* (make-empty-string))
    )
  )
)

;; Utils

;  list related

(defun push-tail (element L)
  "Recursively pushes a new cons to the tail of list."
  (if 
    (null (cdr L))
    (setf (cdr L) (cons  element nil))
    (push-tail element (cdr L))        ; TODO: iteration instead?!?!?!
  )
)

;;;==============================
;; backend translator

(defun mir-to-lang (p)
  "Translator from Markup Intermediate Representation to what-ever language defined by the hash-table in *lang-definition-ht*."
  (cond 
    ((null p))
    ((not (listp  p))
     (concatenate        ; necessary as a separator if there is no closing tag
       'string 
       p 
       (car (nth-value 0 (gethash :text-suffix *lang-definition-ht*)))
     )
    )
    (t 
      (let* 
        (
          (tag (car p))
          (prefix (car (nth-value 0 (gethash tag *lang-definition-ht*))))
          (suffix (car (cdr (nth-value 0 (gethash tag *lang-definition-ht*)))))
        )
        (concatenate 'string prefix (extract p) suffix)
      )
    )
  )
)

(defun extract (p)
  (let ((st ""))
    (loop for i in (cdr p) do 
      (setf st 
        (concatenate 'string st (mir-to-lang i))
      )
    )
    st
  )
)

;; language definition tables

;; HTML
(setf html (make-hash-table))
(setf (gethash :document html) (list "<body>" "</body>"))
(setf (gethash :paragraph html) (list "<p>" "</p>"))
(setf (gethash :text html) (list "" ""))
(setf (gethash :ol html) (list "<ol>" "</ol>"))
(setf (gethash :rgb html) (list "<color>" "</color>"))
(setf (gethash :ul html) (list "<ul>" "</ul>"))
(setf (gethash :li html) (list "<li>" "</li>"))
(setf (gethash :esc html) (list "" ""))
(setf (gethash :unknown-tag html) (list "<span style=\"background-color=red;\">" "</span>"))
(setf (gethash :text-suffix html) (list ""))

(setf pseudom (make-hash-table))
(setf (gethash :document pseudom) (list ":document " nil))
(setf (gethash :paragraph pseudom) (list ":paragraph " nil))
(setf (gethash :text pseudom) (list "<" ">"))
(setf (gethash :ol pseudom) (list ":ordered-list " nil))
(setf (gethash :rgb pseudom)(list ":rbg " nil))
(setf (gethash :ul pseudom) (list ":unordered-list " nil))
(setf (gethash :li pseudom) (list ":list-item " nil))
(setf (gethash :esc pseudom) (list "\\" nil))
(setf (gethash :unknown-tag pseudom) (list ":oops " nil))
(setf (gethash :text-suffix pseudom) (list ""))

;; temp aux stuff

(setf q (list :document (list :ol (list :li (list :paragraph "cobol")) (list :li (list :paragraph "lisp")))))
(setf p "(:DOCUMENT (:OL (:LI (:PARAGRAPH \"cobol\")) (:LI (:PARAGRAPH \"lisp\"))))")
