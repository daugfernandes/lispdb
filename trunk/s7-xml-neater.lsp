;;;  s7-xml-neater.lsp
;;
;;   XML neater
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


(defvar *document* nil 
  "Parsed document root node.")

(defvar *tree* nil      
  "Stack of elements. (car *tree*) -> element in use.")

(defvar *state* nil
  "Current state processing function.")

(defvar *current-string* nil
  "Characters processed so far.")

(defconstant *escaped-chars* "\\{}*"
  "Characters that need special prefixing.")

(defconstant *eol* #\Linefeed
  "End-of-line character.")

(defconstant *void-chars* (string #\Return)
  "Characters that should be ignored.")

(defconstant *start-tag* (character "<"))
(defconstant *end-tag* (character ">"))

(defun is-escaped-char (c)
  (not (null (search (string c) *escaped-chars*)))
)

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
	 (funcall *state* c))
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

(defun i-state (c)
  "Initial state. Nothing interesting happened, yet."
  (print "i")
  (cond
   ((char= c *start-tag*)
    (setf *state* #'t1-state))
   (t ;error
    (error "Invalid start!"))
   )
  )

(defun t1-state (c)
  "aa"
  (print "t1")
  (cond
   ((char= c *end-tag*)
    (error "Invalid start for a TAG."))
   ((char= c #\/)
    (setf *state* #'t2-state))
   (t
    (add-char c)
    (setf *state* #'t4-state))
   )
  )

(defun t2-state (c)
  "aa"
  (print "t2")
  (cond
   ((char= c *end-tag*)
    (pop *tree*)
    (setf *state* #'t3-state))
   ((not (char= #\Space))
    (error "Invalid char after ending /."))
   )
  )

(defun t3-state (c)
  "aa"
  (print "t3")
  (setf *state* #'i-state)
)

(defun t4-state (c)
  "aa"
  (print "t4")
  (cond
   ((char= c #\Space)
    (make-node (list :tag))
    (use-chars-read)
    (setf *state* #'t5-state))
   ((char= c *end-tag*)
    (make-node (list :tag))
    (use-chars-read)
    ;(pop *tree*)
    (setf *state* #'t6-state))
   (t
    (add-char c))
   )
  )

(defun t5-state (c)
  "aa"
  (print "t5")
  (cond
   ((char= c #\/)
    (setf *state* #'t2-state))
   ((char= c *end-tag*)
    (setf *state* #'t6-state))
   )
  )

(defun t6-state (c)
  "aa"
  (print "t6")
  (cond 
   ((char= c *start-tag*)
    (setf *current-string* (make-empty-string))
    (setf *state* #'t1-state))
   (t
    (add-char c)
    (setf *state* #'t8-state))
   )
  )

(defun t8-state (c)
  "aa"
  (print "t8")
  (cond
   ((char= c #\/)
    (setf *state* #'t2-state))
   ((char= c *start-tag*)
    (setf *state* #'t1-state))
   ((char= c *end-tag*)
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

(defmacro t1 () `(test-file "01_empty.txt"))
(defmacro t2 () `(test-file "02_simple_paragraph.txt"))
(defmacro t3 () `(test-file "03_multiline_paragraph.txt"))
(defmacro t4 () `(test-file "04_two_paragraphs.txt"))
(defmacro t5 () `(test-file "05_several_multiline_paragraphs.txt"))
(defmacro t6 () `(test-file "06_header.txt"))
(defmacro t7 () `(test-file "07_headers.txt"))
(defmacro t8 () `(test-file "08_crazy_header.txt"))
(defmacro t9 () `(test-file "09_headers_and_paragraphs.txt"))
(defmacro t10() `(test-file "10_blockquote.txt"))
(defmacro t11() `(test-file "11_multiline_blockquote.txt"))
(defmacro t12() `(test-file "12_multi_paragraph_blockquote.txt"))
(defmacro t13() `(test-file "13_paragraphs_and_blockquotes.txt"))
(defmacro t14() `(test-file "14_simple_verbatim.txt"))
(defmacro t15() `(test-file "15_useful_verbatim.txt"))
(defmacro t16() `(test-file "16_verbatim_with_indentation.txt"))
(defmacro t17() `(test-file "17_verbatim_first_line_extra_indented.txt"))
(defmacro t18() `(test-file ""))
(defmacro t19() `(test-file ""))
(defmacro t20() `(test-file ""))
(defmacro t21() `(test-file ""))
(defmacro t22() `(test-file ""))
(defmacro t23() `(test-file ""))
(defmacro t24() `(test-file ""))
(defmacro t25() `(test-file ""))
(defmacro t26() `(test-file ""))
(defmacro t27() `(test-file ""))
(defmacro t28() `(test-file ""))
(defmacro t29() `(test-file ""))
(defmacro t30() `(test-file ""))
(defmacro t31() `(test-file ""))
(defmacro t32() `(test-file ""))


;; utests
(defun ut ()
  (assert 
   (equal 
    (t1) 
    (list :document (list :body))))
  (assert 
   (equal 
    (t2) 
    (list :document (list :body (list :paragraph (list :text "This is a simple paragraph."))))))
  (assert 
   (equal 
    (t3) 
    (list :document (list :body (list :paragraph (list :text "This is a multiline paragraph.") (list :text "It crosses multiple lines."))))))
  (assert 
   (equal 
    (t4)
    (list :document (list :body (list :paragraph (list :text "This is paragraph number one.")) (list :paragraph (list :text "This is paragraph number two."))))))
  (assert 
   (equal 
    (t5)
    (list :DOCUMENT
	  (list :body
		(list :PARAGRAPH 
		      (list :TEXT "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer")
		      (list :TEXT "dapibus ultricies justo, ut convallis sapien condimentum vel. Praesent")
		      (list :TEXT "interdum, ipsum vitae luctus placerat, turpis risus ornare tellus,")
		      (list :TEXT "eget dictum elit libero sit amet enim. Phasellus malesuada libero at")
		      (list :TEXT "sapien bibendum pretium. Sed ut ligula nunc, consectetur sodales urna.")
		      (list :TEXT "Integer nibh arcu, consectetur id dapibus a, blandit a urna. Curabitur")
		      (list :TEXT "tincidunt vulputate tellus, quis faucibus leo vestibulum ut. Phasellus")
		      (list :TEXT "luctus sem vel turpis pharetra in consequat tortor tincidunt. Duis")
		      (list :TEXT "feugiat nibh eu odio malesuada et eleifend sem sodales. Quisque tempus")
		      (list :TEXT "lobortis condimentum. Pellentesque tempus leo at ante sodales a")
		      (list :TEXT "molestie metus feugiat. Fusce et nisi non mauris pretium euismod.")
		      (list :TEXT "Proin nisl turpis, ornare id molestie tempor, tincidunt varius ligula.")
		      (list :TEXT "Sed at commodo ligula. Aliquam congue aliquam aliquet. Nam bibendum")
		      (list :TEXT "pretium lectus nec interdum. Curabitur a quam id velit lobortis rutrum")
		      (list :TEXT "ac nec massa. Aliquam sit amet risus ligula, at euismod sem. Phasellus")
		      (list :TEXT "odio mi, pretium nec condimentum in, fermentum convallis mi."))
		(list :PARAGRAPH 
		      (list :TEXT "Phasellus at quam arcu, a scelerisque lectus. Quisque magna sem,")
		      (list :TEXT "lobortis vel vulputate ut, ultricies quis urna. Aenean nec lectus")
		      (list :TEXT "nisl. Quisque non sem egestas massa posuere venenatis auctor quis")
		      (list :TEXT "lacus. Donec consequat, velit a scelerisque convallis, diam velit")
		      (list :TEXT "rhoncus felis, sit amet ornare felis felis non sapien. Aliquam erat")
		      (list :TEXT "volutpat. Vestibulum ante ipsum primis in faucibus orci luctus et")
		      (list :TEXT "ultrices posuere cubilia Curae; Sed hendrerit lacinia lectus, et")
		      (list :TEXT "accumsan ante facilisis et. Fusce in mauris ligula, eget placerat")
		      (list :TEXT "odio. Donec aliquet consequat diam, venenatis viverra nulla suscipit") 
		      (list :TEXT "vel."))
		(list :PARAGRAPH 
		      (list :TEXT "Aenean non mauris vel dui pulvinar molestie. Mauris mauris tortor,")
		      (list :TEXT "scelerisque vitae sodales at, egestas ac dolor. Suspendisse potenti.")
		      (list :TEXT "Nunc mollis erat ut lectus interdum tincidunt. Donec diam eros,")
		      (list :TEXT "viverra sit amet posuere et, congue quis arcu. Vestibulum ac odio id")
		      (list :TEXT "nulla posuere adipiscing in quis arcu. Phasellus sed magna sed erat")
		      (list :TEXT "hendrerit consequat. Vivamus eget nisi eget neque hendrerit rhoncus")
		      (list :TEXT "eget quis felis. Ut ut nibh nisi. Phasellus vehicula consequat dui a")
		      (list :TEXT "condimentum. Vestibulum pulvinar sollicitudin tortor at euismod. Donec")
		      (list :TEXT "eget velit felis. Curabitur tempor tristique augue, vel eleifend mi") 
		      (list :TEXT "posuere et."))
		(list :PARAGRAPH 
		      (list :TEXT "Cum sociis natoque penatibus et magnis dis parturient montes, nascetur")
		      (list :TEXT "ridiculus mus. Nunc imperdiet aliquet quam, in sagittis leo accumsan")
		      (list :TEXT "eu. Cras ut lectus risus. In iaculis laoreet eros, a ultricies justo")
		      (list :TEXT "pharetra ac. Suspendisse egestas consequat mauris id porta.")
		      (list :TEXT "Suspendisse euismod luctus arcu vel accumsan. Ut id neque consequat")
		      (list :TEXT "mauris aliquet accumsan eget sed justo. Vestibulum lobortis interdum")
		      (list :TEXT "enim, vitae ultrices massa aliquam at. Maecenas eu purus egestas metus")
		      (list :TEXT "aliquet mollis a ac quam. Praesent vitae tellus eu sapien fringilla")
		      (list :TEXT "venenatis quis sit amet libero. Maecenas id malesuada dui. Suspendisse")
		      (list :TEXT "porta aliquet nisi. Class aptent taciti sociosqu ad litora torquent") 
		      (list :TEXT "per conubia nostra, per inceptos himenaeos."))
		(list :PARAGRAPH 
		      (list :TEXT "Mauris vitae dignissim felis. Proin interdum ullamcorper turpis")
		      (list :TEXT "malesuada euismod. Nam sit amet enim dui, nec congue nibh. Maecenas")
		      (list :TEXT "sapien sem, imperdiet id convallis in, gravida at urna. Curabitur")
		      (list :TEXT "interdum scelerisque libero, semper tempor metus pharetra a. Mauris eu")
		      (list :TEXT "lorem eget arcu scelerisque sollicitudin sit amet eu libero. Sed")
		      (list :TEXT "auctor sagittis libero ac sagittis. Vestibulum a tellus ante, quis")
		      (list :TEXT "eleifend sem. Suspendisse imperdiet velit at justo fermentum vel")
		      (list :TEXT "facilisis nibh tempus. Suspendisse porta, ipsum non aliquet consequat,")
		      (list :TEXT "orci purus viverra nisi, in suscipit elit nulla at neque. Aenean")
		      (list :TEXT "lobortis, est sed rutrum dapibus, mauris nulla suscipit nulla, vitae") 
		      (list :TEXT "dictum leo ligula ut nisi."))))))

  (assert 
   (equal 
    (t6)
    (list :DOCUMENT (list :BODY (list :HEADER 1 (list :TEXT "This is a top level header"))))))

  (assert 
   (equal 
    (t7)
    (list :DOCUMENT
	  (list :BODY (list :HEADER 1 (list :TEXT "This is a primary header.")) (list :HEADER 2 (list :TEXT "This is a secondary header."))
		(list :HEADER 3 (list :TEXT "This is a tertiary header.")) (list :HEADER 4 (list :TEXT "This is a quaternary header."))
		(list :HEADER 5 (list :TEXT "This is a quinary header.")) (list :HEADER 6 (list :TEXT "This is a senary header."))
		(list :HEADER 7 (list :TEXT "This is a septenary header.")) (list :HEADER 8 (list :TEXT "This is a octonary header."))
		(list :HEADER 9 (list :TEXT "This is a nonary header.")) (list :HEADER 10 (list :TEXT "This is a denary header."))
		(list :HEADER 11 (list :TEXT "There's no name for what kind of header this is.")) (list :HEADER 12 (list :TEXT "This is a duodenary header."))))))


  (assert 
   (equal 
    (t8)
    (list :DOCUMENT (list :BODY (list :HEADER 100 (list :TEXT "This is a very low-level header"))))))

  (assert 
   (equal 
    (t9)
    (list :DOCUMENT
	  (list :BODY (list :HEADER 1 (list :TEXT "Header 1.1"))
		(list :PARAGRAPH (list :TEXT "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer")
		      (list :TEXT "dapibus ultricies justo, ut convallis sapien condimentum vel. Praesent")
		      (list :TEXT "interdum, ipsum vitae luctus placerat, turpis risus ornare tellus,")
		      (list :TEXT "eget dictum elit libero sit amet enim. Phasellus malesuada libero at")
		      (list :TEXT "sapien bibendum pretium. Sed ut ligula nunc, consectetur sodales urna.")
		      (list :TEXT "Integer nibh arcu, consectetur id dapibus a, blandit a urna. Curabitur")
		      (list :TEXT "tincidunt vulputate tellus, quis faucibus leo vestibulum ut. Phasellus")
		      (list :TEXT "luctus sem vel turpis pharetra in consequat tortor tincidunt. Duis")
		      (list :TEXT "feugiat nibh eu odio malesuada et eleifend sem sodales. Quisque tempus")
		      (list :TEXT "lobortis condimentum. Pellentesque tempus leo at ante sodales a")
		      (list :TEXT "molestie metus feugiat. Fusce et nisi non mauris pretium euismod.")
		      (list :TEXT "Proin nisl turpis, ornare id molestie tempor, tincidunt varius ligula.")
		      (list :TEXT "Sed at commodo ligula. Aliquam congue aliquam aliquet. Nam bibendum")
		      (list :TEXT "pretium lectus nec interdum. Curabitur a quam id velit lobortis rutrum")
		      (list :TEXT "ac nec massa. Aliquam sit amet risus ligula, at euismod sem. Phasellus")
		      (list :TEXT "odio mi, pretium nec condimentum in, fermentum convallis mi."))
		(list :HEADER 2 (list :TEXT "Header 2.1"))
		(list :PARAGRAPH (list :TEXT "Phasellus at quam arcu, a scelerisque lectus. Quisque magna sem,")
		      (list :TEXT "lobortis vel vulputate ut, ultricies quis urna. Aenean nec lectus")
		      (list :TEXT "nisl. Quisque non sem egestas massa posuere venenatis auctor quis")
		      (list :TEXT "lacus. Donec consequat, velit a scelerisque convallis, diam velit")
		      (list :TEXT "rhoncus felis, sit amet ornare felis felis non sapien. Aliquam erat")
		      (list :TEXT "volutpat. Vestibulum ante ipsum primis in faucibus orci luctus et")
		      (list :TEXT "ultrices posuere cubilia Curae; Sed hendrerit lacinia lectus, et")
		      (list :TEXT "accumsan ante facilisis et. Fusce in mauris ligula, eget placerat")
		      (list :TEXT "odio. Donec aliquet consequat diam, venenatis viverra nulla suscipit")
		      (list :TEXT "vel."))
		(list :HEADER 2 (list :TEXT "Header 2.2"))
		(list :PARAGRAPH (list :TEXT "Aenean non mauris vel dui pulvinar molestie. Mauris mauris tortor,")
		      (list :TEXT "scelerisque vitae sodales at, egestas ac dolor. Suspendisse potenti.")
		      (list :TEXT "Nunc mollis erat ut lectus interdum tincidunt. Donec diam eros,")
		      (list :TEXT "viverra sit amet posuere et, congue quis arcu. Vestibulum ac odio id")
		      (list :TEXT "nulla posuere adipiscing in quis arcu. Phasellus sed magna sed erat")
		      (list :TEXT "hendrerit consequat. Vivamus eget nisi eget neque hendrerit rhoncus")
		      (list :TEXT "eget quis felis. Ut ut nibh nisi. Phasellus vehicula consequat dui a")
		      (list :TEXT "condimentum. Vestibulum pulvinar sollicitudin tortor at euismod. Donec")
		      (list :TEXT "eget velit felis. Curabitur tempor tristique augue, vel eleifend mi") 
		      (list :TEXT "posuere et."))
		(list :HEADER 1 (list :TEXT "Header 1.2"))
		(list :PARAGRAPH (list :TEXT "Cum sociis natoque penatibus et magnis dis parturient montes, nascetur")
		      (list :TEXT "ridiculus mus. Nunc imperdiet aliquet quam, in sagittis leo accumsan")
		      (list :TEXT "eu. Cras ut lectus risus. In iaculis laoreet eros, a ultricies justo")
		      (list :TEXT "pharetra ac. Suspendisse egestas consequat mauris id porta.")
		      (list :TEXT "Suspendisse euismod luctus arcu vel accumsan. Ut id neque consequat")
		      (list :TEXT "mauris aliquet accumsan eget sed justo. Vestibulum lobortis interdum")
		      (list :TEXT "enim, vitae ultrices massa aliquam at. Maecenas eu purus egestas metus")
		      (list :TEXT "aliquet mollis a ac quam. Praesent vitae tellus eu sapien fringilla")
		      (list :TEXT "venenatis quis sit amet libero. Maecenas id malesuada dui. Suspendisse")
		      (list :TEXT "porta aliquet nisi. Class aptent taciti sociosqu ad litora torquent") 
		      (list :TEXT "per conubia nostra, per inceptos himenaeos."))
		(list :PARAGRAPH (list :TEXT "Mauris vitae dignissim felis. Proin interdum ullamcorper turpis")
		      (list :TEXT "malesuada euismod. Nam sit amet enim dui, nec congue nibh. Maecenas")
		      (list :TEXT "sapien sem, imperdiet id convallis in, gravida at urna. Curabitur")
		      (list :TEXT "interdum scelerisque libero, semper tempor metus pharetra a. Mauris eu")
		      (list :TEXT "lorem eget arcu scelerisque sollicitudin sit amet eu libero. Sed")
		      (list :TEXT "auctor sagittis libero ac sagittis. Vestibulum a tellus ante, quis")
		      (list :TEXT "eleifend sem. Suspendisse imperdiet velit at justo fermentum vel")
		      (list :TEXT "facilisis nibh tempus. Suspendisse porta, ipsum non aliquet consequat,")
		      (list :TEXT "orci purus viverra nisi, in suscipit elit nulla at neque. Aenean")
		      (list :TEXT "lobortis, est sed rutrum dapibus, mauris nulla suscipit nulla, vitae") 
		      (list :TEXT "dictum leo ligula ut nisi."))))))
  
  (assert 
   (equal 
    (t10)
    (list :DOCUMENT 
	  (list :BODY (list :QUOTE (list :PARAGRAPH (list :TEXT "This is a blockquote paragraph")))))))
  
  (assert 
   (equal 
    (t11)
    (list :DOCUMENT 
	  (list :BODY (list :QUOTE (list :PARAGRAPH (list :TEXT "This is a blockquote") (list :text "that spans multiple lines.")))))))

  (assert 
   (equal 
    (t12)
    (list :DOCUMENT
	  (list :BODY
		(list :QUOTE 
		      (list :PARAGRAPH (list :TEXT "This is a blockquote.")) 
		      (list :PARAGRAPH (list :TEXT "This is a second paragraph in a blockquote."))
		      (list :PARAGRAPH (list :TEXT "This is a third paragraph in the blockquote.")))))))

  (assert 
   (equal 
    (t13)
    (list :DOCUMENT
	  (list :BODY
		(list :PARAGRAPH 
		      (list :TEXT "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer")
		      (list :TEXT "dapibus ultricies justo, ut convallis sapien condimentum vel. Praesent")
		      (list :TEXT "interdum, ipsum vitae luctus placerat, turpis risus ornare tellus,")
		      (list :TEXT "eget dictum elit libero sit amet enim. Phasellus malesuada libero at")
		      (list :TEXT "sapien bibendum pretium. Sed ut ligula nunc, consectetur sodales urna.")
		      (list :TEXT "Integer nibh arcu, consectetur id dapibus a, blandit a urna. Curabitur")
		      (list :TEXT "tincidunt vulputate tellus, quis faucibus leo vestibulum ut. Phasellus")
		      (list :TEXT "luctus sem vel turpis pharetra in consequat tortor tincidunt. Duis")
		      (list :TEXT "feugiat nibh eu odio malesuada et eleifend sem sodales. Quisque tempus")
		      (list :TEXT "lobortis condimentum. Pellentesque tempus leo at ante sodales a")
		      (list :TEXT "molestie metus feugiat. Fusce et nisi non mauris pretium euismod.")
		      (list :TEXT "Proin nisl turpis, ornare id molestie tempor, tincidunt varius ligula.")
		      (list :TEXT "Sed at commodo ligula. Aliquam congue aliquam aliquet. Nam bibendum")
		      (list :TEXT "pretium lectus nec interdum. Curabitur a quam id velit lobortis rutrum")
		      (list :TEXT "ac nec massa. Aliquam sit amet risus ligula, at euismod sem. Phasellus")
		      (list :TEXT "odio mi, pretium nec condimentum in, fermentum convallis mi."))
		(list :QUOTE
		      (list :PARAGRAPH 
			    (list :TEXT "Phasellus at quam arcu, a scelerisque lectus. Quisque magna sem,")
			    (list :TEXT "lobortis vel vulputate ut, ultricies quis urna. Aenean nec lectus")
			    (list :TEXT "nisl. Quisque non sem egestas massa posuere venenatis auctor quis")
			    (list :TEXT "lacus. Donec consequat, velit a scelerisque convallis, diam velit")
			    (list :TEXT "rhoncus felis, sit amet ornare felis felis non sapien. Aliquam erat")
			    (list :TEXT "volutpat. Vestibulum ante ipsum primis in faucibus orci luctus et")
			    (list :TEXT "ultrices posuere cubilia Curae; Sed hendrerit lacinia lectus, et")
			    (list :TEXT "accumsan ante facilisis et. Fusce in mauris ligula, eget placerat")
			    (list :TEXT "odio. Donec aliquet consequat diam, venenatis viverra nulla suscipit") 
			    (list :TEXT "vel.")))
		(list :PARAGRAPH (list :TEXT "Aenean non mauris vel dui pulvinar molestie. Mauris mauris tortor,")
		      (list :TEXT "scelerisque vitae sodales at, egestas ac dolor. Suspendisse potenti.")
		      (list :TEXT "Nunc mollis erat ut lectus interdum tincidunt. Donec diam eros,")
		      (list :TEXT "viverra sit amet posuere et, congue quis arcu. Vestibulum ac odio id")
		      (list :TEXT "nulla posuere adipiscing in quis arcu. Phasellus sed magna sed erat")
		      (list :TEXT "hendrerit consequat. Vivamus eget nisi eget neque hendrerit rhoncus")
		      (list :TEXT "eget quis felis. Ut ut nibh nisi. Phasellus vehicula consequat dui a")
		      (list :TEXT "condimentum. Vestibulum pulvinar sollicitudin tortor at euismod. Donec")
		      (list :TEXT "eget velit felis. Curabitur tempor tristique augue, vel eleifend mi") 
		      (list :TEXT "posuere et."))
		(list :QUOTE
		      (list :PARAGRAPH 
			    (list :TEXT "Cum sociis natoque penatibus et magnis dis parturient montes,")
			    (list :TEXT "nascetur ridiculus mus. Nunc imperdiet aliquet quam, in sagittis leo")
			    (list :TEXT "accumsan eu. Cras ut lectus risus. In iaculis laoreet eros, a")
			    (list :TEXT "ultricies justo pharetra ac. Suspendisse egestas consequat mauris id")
			    (list :TEXT "porta. Suspendisse euismod luctus arcu vel accumsan. Ut id neque")
			    (list :TEXT "consequat mauris aliquet accumsan eget sed justo. Vestibulum")
			    (list :TEXT "lobortis interdum enim, vitae ultrices massa aliquam at. Maecenas eu")
			    (list :TEXT "purus egestas metus aliquet mollis a ac quam. Praesent vitae tellus")
			    (list :TEXT "eu sapien fringilla venenatis quis sit amet libero. Maecenas id")
			    (list :TEXT "malesuada dui. Suspendisse porta aliquet nisi. Class aptent taciti")
			    (list :TEXT "sociosqu ad litora torquent per conubia nostra, per inceptos") 
			    (list :TEXT "himenaeos."))
		      (list :PARAGRAPH 
			    (list :TEXT "Mauris vitae dignissim felis. Proin interdum ullamcorper turpis")
			    (list :TEXT "malesuada euismod. Nam sit amet enim dui, nec congue nibh. Maecenas")
			    (list :TEXT "sapien sem, imperdiet id convallis in, gravida at urna. Curabitur")
			    (list :TEXT "interdum scelerisque libero, semper tempor metus pharetra a. Mauris")
			    (list :TEXT "eu lorem eget arcu scelerisque sollicitudin sit amet eu libero. Sed")
			    (list :TEXT "auctor sagittis libero ac sagittis. Vestibulum a tellus ante, quis")
			    (list :TEXT "eleifend sem. Suspendisse imperdiet velit at justo fermentum vel")
			    (list :TEXT "facilisis nibh tempus. Suspendisse porta, ipsum non aliquet")
			    (list :TEXT "consequat, orci purus viverra nisi, in suscipit elit nulla at neque.")
			    (list :TEXT "Aenean lobortis, est sed rutrum dapibus, mauris nulla suscipit") 
			    (list :TEXT "nulla, vitae dictum leo ligula ut nisi.")))
		(list :PARAGRAPH 
		      (list :TEXT "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec sodales")
		      (list :TEXT "cursus sagittis. Proin eu mattis tortor. Duis egestas viverra ornare.")
		      (list :TEXT "Vivamus convallis nisi a enim luctus auctor. Sed rutrum facilisis")
		      (list :TEXT "risus dapibus sagittis. Proin id ligula ipsum, vitae fermentum enim.")
		      (list :TEXT "Mauris rutrum, ante a lobortis tempus, libero nulla imperdiet enim,")
		      (list :TEXT "vel accumsan sapien nulla sed sem. Nunc scelerisque, odio quis feugiat")
		      (list :TEXT "faucibus, nisl nisl adipiscing ligula, a ullamcorper sapien metus in")
		      (list :TEXT "sem. Ut tempor nisi vel eros rhoncus eu vulputate dui tempor. Cras nec")
		      (list :TEXT "diam neque. Nullam ac est et est tincidunt suscipit. Proin vitae velit")
		      (list :TEXT "vitae ante accumsan tristique. Donec justo nulla, consequat nec")
		      (list :TEXT "vestibulum a, molestie quis magna. Nulla vitae placerat neque. Proin")
		      (list :TEXT "laoreet, mauris sed accumsan ornare, ipsum elit feugiat mi, in")
		      (list :TEXT "fringilla dolor leo at felis. Vestibulum ante ipsum primis in faucibus") 
		      (list :TEXT "orci luctus et ultrices posuere cubilia Curae;"))))))

  (assert 
   (equal 
    (t14)
    (list :DOCUMENT 
	  (list :BODY (list :VERBATIM (list :text "This is simple verbatim text."))))))
)


;; language definition tables

;; HTML
(setf html (make-hash-table))
(setf (gethash :document html) (list t (format nil "~%<html>~%") t (format nil "~%</html>~%")))
(setf (gethash :body html) (list t (format nil "<body>~%") t (format nil "</body>~%")))
(setf (gethash :paragraph html) (list t "<p>" t (format nil "</p>~%")))
(setf (gethash :text html) (list nil "" nil ""))
(setf (gethash :ol html) (list t "<ol>" t "</ol>"))
(setf (gethash :rgb html) (list t "<color>" t "</color>"))
(setf (gethash :ul html) (list t "<ul>" t "</ul>"))
(setf (gethash :li html) (list t "<li>" t "</li>"))
(setf (gethash :esc html) (list t "" t ""))
(setf (gethash :unknown-tag html) (list t "<span style=\"background-color=red;\">" t "</span>"))
(setf (gethash :text-suffix html) (list ""))

(setf pseudom (make-hash-table))
(setf (gethash :document pseudom) (list ":document " nil))
(setf (gethash :body pseudom) (list ":body " nil))
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
