(in-package :common-lisp-user)

(defpackage :s7-markup-parser
  (:use :common-lisp)
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

(defun s7m-to-html (doc)
)


(defun extract (p)
  (if
    (not (null p))
      (cond 
        ((not (listp p))
         (list p))
        ((eq (car p) :document) 
         (list "<body>" (extract (car (cdr p))) "</body>"))
        ((eq (car p) :ol)
         (list "<ol>" (extract (car (cdr p))) "</ol>"))
        ((eq (car p) :li)
         (list "<li>" (extract (car (cdr p))) "</li>"))
      )
  )
)