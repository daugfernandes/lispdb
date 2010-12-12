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

(defvar *current-state* nil
  "Function with current-state.")

(defvar *document* nil 
  "Parsed document root node.")

(defvar *tree* nil      
  "Document pushdown tree. (car *tree*) is the last element in use.")

(defvar *states* nil
  "Pushdown list os states so far.")

(defvar *current-string* nil
  "Characters processed so far.")

(defconstant *escaped-chars* "\\{}*"
  "Characters that need special prefixing.")
(defconstant *eol* #\.
  "End-of-line character.")
(defconstant *void-chars* (string #\Return)
  "Characters that should be ignored.")

(defun is-escaped-char (c)
  (not (null (search (string c) *escaped-chars*)))
)

(defun parse-stream (stream)
  "Parse a stream of characters."
  (setf *state* #'document
        *document* (list :document)
        *tree* (list *document*)
        *current-string* (make-empty-string))
  (push :document *states*)
  (catch 'end-of-file
    (loop
      (let ((c (read-char stream nil :eof)))
        (if (null (search (string c) *void-chars*))
          (funcall *state* c)
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

(defun change-state (state)
  "Changes the actual state-processing function."
  (setf *state*
    (ecase state
      (:paragraph      #'paragraph)
      (:ordered-list   #'ordered-list)
      (:unordered-list #'unordered-list)
      (:list-item      #'list-item)
      (:link           #'link)
      (:document       #'document)
      (:rgb            #'rgb)
      (:text           #'text)
      (:line-feed      #'first-linefeed)
      (:esc            #'escape)
    )
  )
)

;; states

(defun document (char)
  "Document state. Nothing interesting happened, yet."
  (cond

    ((test-eof char))

    ((char= char *eol*)
      (cond 
        ((zerop (length *current-string*))
          (make-node (list :paragraph))
          (pop *tree*)
        )
      (t (use-chars-read)))
    )

    ((is-escaped-char char)
      (use-chars-read)
      (make-node (list :esc))
      (push :esc *states*)
      (add-char char)
      (use-chars-read)
      (pop *tree*)
      (pop *states*)
      (change-state (car *states*))
      (make-node (list :text))
    )

    ((char= char #\#)
      (make-node (list :rgb))
      (change-state :rgb)
      (push :rgb *states*)
      (add-char char))

    (t
      (make-node (list :paragraph))
      (change-state :paragraph)
      (push :paragraph *states*)
      (make-node (list :text))
      (change-state :text)
      (push :text *states*)
      (add-char char))

  )
)

(defun rgb (char)
  (cond 

    ((test-eof char))

    ((not (char-hexa char))
      (use-chars-read)
      (pop *tree*)
      (pop *states*)
      (change-state (car *states*))
      (make-node (list :text))
    )
  )
  (add-char char)
)

(defun paragraph (char)
  (cond

    ((test-eof char))

    ((char= char *eol*)
      (push :line-feed *states*)
      (change-state :line-feed))

    ((char= char #\#)
      (use-chars-read)
      (make-node (list :rgb))
      (push :rgb *states*)
      (change-state :rgb))

    (t 
      (make-node (list :text))
      (push :text *states*)
      (change-state :text)
      (add-char char))
  )
)

(defun ordered-list (char)
  (cond
    ((test-eof char))
    (t (make-node (list :list-item))
      (make-node (list :paragraph))
      (change-state :paragraph)
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

(defun text (char)
  (cond
    ((test-eof char))
    ((char= char *eol*)
      (push :line-feed *states*)
      (change-state :line-feed)
      ;(use-chars-read)
      ;(pop *tree*)
      ;(pop *states*)
      ;(if (null *states*) (push :document *states))
      ;(change-state (car *states*))
    )

    ((is-escaped-char char)
      (use-chars-read)
      (pop *tree*)
      (make-node (list :esc))
      (add-char char)
      (use-chars-read)
      (pop *tree*)
      (pop *states*)
      (change-state (car *states*))
    )

    ((char= char #\#)
      (use-chars-read)
      (make-node (list :rgb))
      (push :rgb *states*)
      (change-state :rgb))

    (t (add-char char))
  )
)

(defun first-linefeed (char)
  "A first Linefeed was found."
  (cond
    ((test-eof char))

    ((char= char *eol*) ;second one
      (use-chars-read)
      (pop *tree*) (pop *tree*)
      (pop *states*) ;removes linefeed state
      (pop *states*) ;removes previous state
      (pop *states*) ;removes previous state
      (if (null *states*) (push :document *states))
      (change-state (car *states*)))

    ((is-escaped-char char)
      (use-chars-read)
      (make-node (list :esc))
      (push :esc *states*)
      (use-chars-read)
      (pop *tree*)
      (pop *states*)
      (change-state (car *states*))
      (make-node (list :text))
    )

    ((char= char #\#)
      (use-chars-read)
      (make-node (list :rgb))
      (push :rgb *states*)
      (change-state :rgb))

    (t 
      (pop *states*)
      (change-state (car *states*))
      (add-char *eol*) ;adds the first linefeed
      (add-char char)
    )
  )
)


(defun make-node (content)
  "Creates a new node." 
  (setf node content)
  (push-tail node (car *tree*)) ;sets it in the document structure
  (push node *tree*)            ;and in the document pushdown tree
)

(defun char-hexa (char)
  "Tests for a hexa char."
  (or
    (and (char>= char #\0) (char<= char #\9))
    (and (char>= char #\A) (char<= char #\F))
    (and (char>= char #\a) (char<= char #\f))
  )
)

(defun test-eof (char)
  (cond 
    ((eq char :eof)
      (use-chars-read)
      (pop *tree*)
      (pop *states*)
      (change-state (car *states*))
      (throw 'end-of-file nil)
    )
  )
)

(defun add-char (char)
  (vector-push-extend char *current-string*))

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
