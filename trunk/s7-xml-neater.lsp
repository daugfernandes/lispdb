;;;  s7-xml-neater.lsp
;;
;;   XML neater
;;
;;   	$Id$	
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


(defvar *document* nil 
  "List-type XML document.")

(defvar *tree* nil
  "Stack of elements. (car *tree*) -> element in use.")

(defvar *state* nil
  "Current state processing function.")

(defvar *current-string* nil
  "Characters processed so far.")

(defvar *ns* nil
  "Namespace waiting to be used.")

(defvar *debuging* t
  "Flag for debuging prints.")

(defvar *definition* nil
  "Flag that will handle first line '<?XML' that doesn't end normaly with />")

(defconstant *eol* #\Linefeed
  "End-of-line character.")

(defun parse-stream (stream)
  "Parse a stream of characters."
  (setf *document* (list :xml)
        *tree* (list *document*)
        *current-string* (make-empty-string)
	*state* #'i-state)

  (catch 'end-of-file
    (loop
     (let ((c (read-char stream nil :eof)))
       (if (eq c :eof)
	   (progn
	     (use-chars-read)
	     (throw 'end-of-file nil))
	   (if 
	    (not (or (char= c *eol*) (char= c #\Return)))
	    (funcall *state* c)))
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


;; predicates

(defmacro char<-p  (c) `(char= ,c #\<))
(defmacro char>-p  (c) `(char= ,c #\>))
(defmacro char/-p  (c) `(char= ,c #\/))
(defmacro char=-p  (c) `(char= ,c #\=))
(defmacro char\:-p (c) `(char= ,c #\:))
(defmacro char\"-p (c) `(char= ,c #\"))
(defmacro space-p  (c) `(char= ,c #\Space))

(defmacro trc (m) `(if *debuging* (print ,m))) 

;; states

(defun i-state (c)
  "Initial state. Nothing interesting happened, yet."
  (trc "i")
  (cond

   ((char<-p c)                     ; ok! first char's a < , good!!
    (setf *state* #'t1-state))

   ((space-p c)                     ; nothing to do.
    (setf *state* #'i-state))

   (t                               ; oops! no good!!
    (error "Invalid start!"))

   )
  )

(defun t1-state (c)
  "TAG starting."
  (trc "t1")
  (cond

   ((space-p c)
    (setf *state* #'t1-state))

   ((char>-p c)                   ; <> is not a very good TAG!
    (error "Invalid start for a TAG."))

   ((char/-p c)                   ; Closing TAG starting
    (setf *state* #'t2a-state))

   (t                             ; TAG's name starting
    (setf *definition* (char= c #\?))
    (add-char c)
    (setf *state* #'t4-state))

   )
  )

(defun t2a-state (c)
  "/TAG like."
  (trc "t2a")
  (cond

    ((char>-p c)
     (if 
       (not (string= (cadar *tree*) *current-string*))
       (error (format t "Mismatch: closing tag /~s with /~s" (cadar *tree*) *current-string*)))
     (setf *current-string* (make-empty-string))
     (pop *tree*)
     (setf *state* #'i-state))

   ((char= c #\:)
    (setf *ns* *current-string*)
    (setf *current-string* (make-empty-string)))

    (t
     (add-char c))))

(defun t2-state (c)
  "Closing TAG starting."
  (trc "t2")
  (cond

   ((char>-p c)            ; TAG's name complete.
    (pop *tree*)
    (setf *state* #'t3-state))

   (t
    (error (format t "Invalid char '~a' after ending /." c)))

   )
  )

(defun t3-state (c)
  "Just an epsilon state."
  (trc "t3")
  (setf *state* #'t1-state)
)

(defun t4-state (c)
  "TAG's name."
  (trc "t4")
  (cond

   ((space-p c)            ; TAG's name completed maybe a ATTR follows
    (make-node (list :tag))
    (use-chars-read)
     (if
      (> (length *ns*) 0)
      (progn
	(push-tail :ns (car *tree*))
	(push-tail *ns* (car *tree*))
	(setf *ns* (make-empty-string))))
    (setf *state* #'t5-state))

   ((char\:-p c)
    (setf *ns* *current-string*)
    (setf *current-string* (make-empty-string)))

   ((char/-p c)            ; TAG's name completed
    (make-node (list :tag))
    (use-chars-read)
     (if
      (> (length *ns*) 0)
      (progn
	(push-tail :ns (car *tree*))
	(push-tail *ns* (car *tree*))
	(setf *ns* (make-empty-string))))
    (setf *state* #'t5-state))

   ((char>-p c)                ; TAG's name completed
    (make-node (list :tag))
    (use-chars-read)
     (if
      (> (length *ns*) 0)
      (progn
	(push-tail :ns (car *tree*))
	(push-tail *ns* (car *tree*))
	(setf *ns* (make-empty-string))))
    (setf *state* #'t6-state))

   (t
    (add-char c))

  )
)

(defun t5-state (c)
  "Maybe Attributes."
  (trc "t5")
  (cond

   ((char/-p c)                        ; closing TAG
    (setf *state* #'t2-state))

   ((char>-p c)                        ; closed TAG
    (if
     *definition*
     (progn
       (pop *tree*)
       (setf *definition* nil)))
    (setf *state* #'t6-state))

   ((space-p c)
    (setf *state* #'t5-state))         ; do nothing

   (t                                  ; attribute's name starting
    (add-char c)
    (setf *state* #'t9-state))

  )
)

(defun t9-state (c)
  "Attribute."
  (trc "t9")
  (cond

    ((char=-p c)                   ; attribute's name complete, starting value
     (make-node (list :attr))
     (use-chars-read)
     (if
      (> (length *ns*) 0)
      (progn
	(push-tail :ns (car *tree*))
	(push-tail *ns* (car *tree*))
	(setf *ns* (make-empty-string))))
     (setf *state* #'t10-state))
    
    ((char= c #\:)
     (setf *ns* *current-string*)
     (setf *current-string* (make-empty-string)))

    ((space-p c)                   ; valueless attribute
     (make-node (list :attr))
     (use-chars-read)
     (pop *tree*)
     (setf *state* #'t5-state))

    ((or (char>-p c) (char/-p c))
     (make-node (list :attr))
     (use-chars-read)
     (pop *tree*) (pop *tree*)
     (setf *state* #'t3-state))

    (t
     (add-char c))
   )
)

(defun t10-state (c)
  "Attribute's value"
  (trc "t10")
  (cond

    ((char\"-p c)
     (setf *state* #'t11-state))

    ((char>-p c)
     (use-chars-read)
     (pop *tree*)
     (setf *state* #'t6-state))

    ((char/-p c)
     (use-chars-read)
     (pop *tree*)
     (setf *state* #'t2-state))

    ((char<-p c)
     (error "Invalid inline < in attr value."))

    ((space-p c)
     (use-chars-read)
     (pop *tree*)
     (setf *state* #'t5-state))

    (t
     (add-char c))

  )
)

(defun t11-state (c)
  "Attribute's value between double-quotes."
  (trc "t11")
  (cond

    ((char\"-p c)
     (use-chars-read)
     (pop *tree*)
     (setf *state* #'t5-state))

    (t
     (add-char c))))

(defun t6-state (c)
  "Inner body of element starting."
  (trc "t6")
  (cond 

   ((char<-p c)
    (setf *current-string* (make-empty-string))
    (setf *state* #'t1-state))

   ((space-p c)
    (setf *state* #'t6-state))

   (t
    (add-char c)
    (setf *state* #'t8-state))

   )
)

(defun t8-state (c)
  "Inner body of element."
  (trc "t8")
  (cond

   ((char/-p c)
    (setf *state* #'t2-state))

   ((char<-p c)
    (use-chars-read)
    (setf *state* #'t1-state))

   ((char>-p c)
    (error "Invalid inline >."))

   (t
    (add-char c))

  )
)

(defun make-node (content)
  "Creates a new node." 
  (setf node content)
  (push-tail node (car *tree*)) ;sets it in the document structure
  (push node *tree*)            ;and in the document pushdown tree
)

(defun char-hexa (c)
  "Tests for a hexa char."
  (or
    (char-numeric c)
    (and (char>= c #\A) (char<= c #\F))
    (and (char>= c #\a) (char<= c #\f))
  )
)

(defun char-alfa (c)
  "Tests for a alphabetic char."
  (or
    (and (char>= c #\A) (char<= c #\Z))
    (and (char>= c #\a) (char<= c #\z))
  )
)

(defun char-numeric (c)
  "Tests for a numeric char."
  (and (char>= c #\0) (char<= c #\9))
)

(defun char-alfanumeric (c)
  "Tests for a alphanumeric char."
  (or
    (char-alfa c)
    (char-numeric c)
  )
)

(defun last-char-of (s)
  (char s (1- (length s)))
)

(defun add-char (c)
  (cond
   ((zerop (length *current-string*))
    (vector-push-extend c *current-string*))
   ((not (char= #\Space c))
    (cond 
     ((char= *eol* (last-char-of *current-string*))
      (vector-pop *current-string*)
      (vector-push-extend #\Space *current-string*)))
    (vector-push-extend c *current-string*))
   ((not (char= *eol* (last-char-of *current-string*)))
    (vector-push-extend c *current-string*)))
)

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
       (car *tree*))
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

(defun save-list (list filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print list out)))
)

;;;==============================
;; backend translator

(defun mir-to-lang (p ident)
  "Translator from Markup Intermediate Representation to what-ever language defined by the hash-table in *lang-definition-ht*."
  (cond 
    ((null p))
    ((not (listp  p))
     (concatenate        ; necessary as a separator if there is no closing tag
       'string 
       p 
       (car (gethash :text-suffix *lang-definition-ht*))
     )
    )
    (t 
      (let* 
        (
          (tag (car p))
          (prefix (get-tag-prefix tag *lang-definition-ht* ident))
          (suffix (get-tag-suffix tag *lang-definition-ht* ident))
        )
	(concatenate 'string ident prefix (extract p ident) suffix))
    )
  )
)

(defun get-tag-prefix (tag ht ident)
  (let ((ret (cadr (gethash tag ht))))
    (cond 
      ((null ret)
       (setf ret (get-tag-prefix :unknown-tag ht ident)))
      ((car (gethash tag ht))
       (setf ret (concatenate 'string *ident-space* ret))))
    ret)
)

(defun get-tag-suffix (tag ht ident)
  (let ((ret (cadddr (gethash tag ht))))
    (cond
      ((null ret)
       (setf ret (get-tag-suffix :unknown-tag ht ident)))
      ((caddr (gethash tag ht))
       (setf ret (concatenate 'string *ident-space* ret))))
    ret)
)

(defun extract (p ident)
  (let ((st ""))
    (loop for i in (cdr p) do 
      (setf st 
        (concatenate 'string st (mir-to-lang i ident))))
    st
  )
)

(defmacro t1 () `(test-string ""))




;; utests
(defun ut ()
  (assert 
   (equal 
    (test-string "")
    (list :xml)))

  (assert 
   (equal 
    (test-string "<html>")
    (list :xml (list :tag "html"))))

  (assert 
   (equal 
    (test-string "<html id=1>")
    (list :xml (list :tag "html" (list :attr "id" "1")))))

  (assert 
   (equal 
    (test-string "<html id=1 name=2>")
    (list :xml (list :tag "html" (list :attr "id" "1") (list :attr "name" "2")))))

  (assert
   (equal
    (test-string "<html id=1 name=\"david fernandes\">")
    (list :xml (list :tag "html" (list :attr "id" "1") (list :attr "name" "david fernandes")))))

  (assert
   (equal
    (test-string "<a u=1 a=2>ii</a>")
    (list :XML (list :TAG "a" (list :ATTR "u" "1") (list :ATTR "a" "2") "ii"))))
)

