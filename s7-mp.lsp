;;;  s7-mp.lsp
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

(defconstant *ident-space* "    "
  "Identation characters.")

(defvar *document* nil 
  "Parsed document root node.")

(defvar *tree* nil      
  "Stack of elements. (car *tree*) -> element in use.")

(defvar *state* nil
  "Current state processing function.")

(defvar *what-if-eol* nil
  "Stack of eol treatment functions.")

(defvar *header-count* 0
  "Counts *")

(defvar *current-string* nil
  "Characters processed so far.")

(defconstant *escaped-chars* "\\{}*"
  "Characters that need special prefixing.")

(defconstant *eol* #\Linefeed
  "End-of-line character.")

(defconstant *void-chars* (string #\Return)
  "Characters that should be ignored.")

(defun is-escaped-char (c)
  (not (null (search (string c) *escaped-chars*)))
)

(defun parse-stream (stream)
  "Parse a stream of characters."
  (setf *document* (list :document)
        *tree* (list *document*)
        *current-string* (make-empty-string)
	*header-count* 0
	*state* #'i-state)
  (make-node (list :body))

  (catch 'end-of-file
    (loop
      (let ((c (read-char stream nil :eof)))
        (if (null (search (string c) *void-chars*))
	    (progn
	      (if (eq c :eof)
		  (progn
		    (use-chars-read)
		    (throw 'end-of-file nil))
		(funcall *state* c))
	    )
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

(defun document ())

(defun i-state (c)
  "Initial state. Nothing interesting happened, yet."
  (cond
    ((char= c #\*)                 ; header starting
     (incf *header-count*)
     (setf *state* #'h1-state)
     (add-char c))
    ((char= c #\Space)             ; first space -> maybe block section
     (setf *state* #'s1-state))
    (t                             ; plain text paragraph
     (setf *state*  #'t-state)
     (make-node (list :paragraph))
     (make-node (list :text))
     (add-char c))
  )
)

(defun h1-state (c)
  "Header-maybe state."
  (cond
    ((char= c #\*)                 ; header +1 level
     (incf *header-count*)
     (add-char c))
    ((char= c #\Space)             ; header confirmation; clean *'s
     (make-node (list :header *header-count*))
     (make-node (list :text))
     (setf *state* #'t-state
           *current-string* (make-empty-string)
           *header-count* 0))
    (t                             ; plain text paragraph <- (*'s w/o space)
     (make-node (list :paragraph))
     (make-node (list :text))
     (setf *state* #'t-state
	   *header-count* p)
     (add-char c))
  )
)

(defun s1-state (c)
  "First space found."
  (cond
    ((char= c #\Space)
     (setf *state* #'bl-state))
    (t
     (make-node (list :paragraph))
     (make-node (list :text))
     (setf *state* #'t-state)
     (add-char c))
  )
)

(defun bl-state (c)
  "Block state."
  (cond
    ((char= c #\Space)
     (setf *state* #'v1-state))
    ((char= c #\#))
    ((char= c #\-))
    (t
     (make-node (list :quote))
     (make-node (list :paragraph))
     (make-node (list :text))
     (setf *state* #'t-state
	   *current-string* (make-empty-string))
     (add-char c))
  )
)

(defun v1-state (c)
  "Maybe VERBATIM."
  (cond
    ((char= c #\Space)
     (setf *state* #'bl-state))
    (t
     (make-node (list :verbatim))
     (setf *state* #'i-state
	   *current-string* (make-empty-string))
     (add-char c))
  )
)

(defun p-state (c)
  "Paragraph state."
  (add-char c)
)

(defun t-state (c)
  "Text state."
  (cond
    ((char= c *eol*)
     (setf *state* #'t1-state))
    (t
     (add-char c))
  )
)

(defun t1-state (c)
  (cond
    ((char= c *eol*)
     (use-chars-read)
     (setf q (caadr *tree*))
     (pop *tree*) (pop *tree*)
     (make-node (list (car *tree*)))
     (setf *state* #'i-state)) ;TODO: change this
    (t
     ;(add-char #\Space)
     (add-char c)
     (setf *state* #'t-state))
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
   ((not (char= *eol* (last-char-of *current-string*)))
    (vector-push-extend c *current-string*))
   ((not (char= #\Space c))
    (cond 
     ((char= *eol* (last-char-of *current-string*))
      (vector-pop *current-string*)
      (vector-push-extend #\Space *current-string*)))
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

;; utests
(defun ut ()
  (assert 
   (equal 
    (test-file "01_empty.txt") 
    (list :document (list :body))))
  (assert 
   (equal 
    (test-file "02_simple_paragraph.txt") 
    (list :document (list :body (list :paragraph (list :text "This is a simple paragraph."))))))
  (assert 
   (equal 
    (test-file "03_multiline_paragraph.txt") 
    (list :document (list :body (list :paragraph (list :text "This is a multiline paragraph. It crosses multiple lines."))))))
  (assert 
   (equal 
    (test-file "04_two_paragraphs.txt")
    (list :document (list :body (list :paragraph (list :text "This is paragraph number one.")) (list :paragraph (list :text "This is paragraph number two."))))))
  (assert 
   (equal 
    (test-file "05_several_multiline_paragraphs.txt")
    (list :DOCUMENT
	  (list :BODY
		(list :PARAGRAPH
		      (list :TEXT
			    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer dapibus ultricies justo, ut convallis sapien condimentum vel. Praesent interdum, ipsum vitae luctus placerat, turpis risus ornare tellus, eget dictum elit libero sit amet enim. Phasellus malesuada libero at sapien bibendum pretium. Sed ut ligula nunc, consectetur sodales urna. Integer nibh arcu, consectetur id dapibus a, blandit a urna. Curabitur tincidunt vulputate tellus, quis faucibus leo vestibulum ut. Phasellus luctus sem vel turpis pharetra in consequat tortor tincidunt. Duis feugiat nibh eu odio malesuada et eleifend sem sodales. Quisque tempus lobortis condimentum. Pellentesque tempus leo at ante sodales a molestie metus feugiat. Fusce et nisi non mauris pretium euismod. Proin nisl turpis, ornare id molestie tempor, tincidunt varius ligula. Sed at commodo ligula. Aliquam congue aliquam aliquet. Nam bibendum pretium lectus nec interdum. Curabitur a quam id velit lobortis rutrum ac nec massa. Aliquam sit amet risus ligula, at euismod sem. Phasellus odio mi, pretium nec condimentum in, fermentum convallis mi."))
		(list :PARAGRAPH
		      (list :TEXT
			    "Phasellus at quam arcu, a scelerisque lectus. Quisque magna sem, lobortis vel vulputate ut, ultricies quis urna. Aenean nec lectus nisl. Quisque non sem egestas massa posuere venenatis auctor quis lacus. Donec consequat, velit a scelerisque convallis, diam velit rhoncus felis, sit amet ornare felis felis non sapien. Aliquam erat volutpat. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Sed hendrerit lacinia lectus, et accumsan ante facilisis et. Fusce in mauris ligula, eget placerat odio. Donec aliquet consequat diam, venenatis viverra nulla suscipit vel."))
		(list :PARAGRAPH
		      (list :TEXT
			    "Aenean non mauris vel dui pulvinar molestie. Mauris mauris tortor, scelerisque vitae sodales at, egestas ac dolor. Suspendisse potenti. Nunc mollis erat ut lectus interdum tincidunt. Donec diam eros, viverra sit amet posuere et, congue quis arcu. Vestibulum ac odio id nulla posuere adipiscing in quis arcu. Phasellus sed magna sed erat hendrerit consequat. Vivamus eget nisi eget neque hendrerit rhoncus eget quis felis. Ut ut nibh nisi. Phasellus vehicula consequat dui a condimentum. Vestibulum pulvinar sollicitudin tortor at euismod. Donec eget velit felis. Curabitur tempor tristique augue, vel eleifend mi posuere et."))
		(list :PARAGRAPH
		      (list :TEXT
			    "Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nunc imperdiet aliquet quam, in sagittis leo accumsan eu. Cras ut lectus risus. In iaculis laoreet eros, a ultricies justo pharetra ac. Suspendisse egestas consequat mauris id porta. Suspendisse euismod luctus arcu vel accumsan. Ut id neque consequat mauris aliquet accumsan eget sed justo. Vestibulum lobortis interdum enim, vitae ultrices massa aliquam at. Maecenas eu purus egestas metus aliquet mollis a ac quam. Praesent vitae tellus eu sapien fringilla venenatis quis sit amet libero. Maecenas id malesuada dui. Suspendisse porta aliquet nisi. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos."))
		(list :PARAGRAPH
		      (list :TEXT
			    "Mauris vitae dignissim felis. Proin interdum ullamcorper turpis malesuada euismod. Nam sit amet enim dui, nec congue nibh. Maecenas sapien sem, imperdiet id convallis in, gravida at urna. Curabitur interdum scelerisque libero, semper tempor metus pharetra a. Mauris eu lorem eget arcu scelerisque sollicitudin sit amet eu libero. Sed auctor sagittis libero ac sagittis. Vestibulum a tellus ante, quis eleifend sem. Suspendisse imperdiet velit at justo fermentum vel facilisis nibh tempus. Suspendisse porta, ipsum non aliquet consequat, orci purus viverra nisi, in suscipit elit nulla at neque. Aenean lobortis, est sed rutrum dapibus, mauris nulla suscipit nulla, vitae dictum leo ligula ut nisi."))))))

  (assert 
   (equal 
    (test-file "06_header.txt")
    (list :DOCUMENT (list :BODY (list :HEADER 1 (list :TEXT "This is a top level header"))))))

  (assert 
   (equal 
    (test-file "07_headers.txt")
(list :DOCUMENT
 (list :BODY (list :HEADER 1 (list :TEXT "This is a primary header.")) (list :HEADER 2 (list :TEXT "This is a secondary header."))
  (list :HEADER 3 (list :TEXT "This is a tertiary header.")) (list :HEADER 4 (list :TEXT "This is a quaternary header."))
  (list :HEADER 5 (list :TEXT "This is a quinary header.")) (list :HEADER 6 (list :TEXT "This is a senary header."))
  (list :HEADER 7 (list :TEXT "This is a septenary header.")) (list :HEADER 8 (list :TEXT "This is a octonary header."))
  (list :HEADER 9 (list :TEXT "This is a nonary header.")) (list :HEADER 10 (list :TEXT "This is a denary header."))
  (list :HEADER 11 (list :TEXT "There's no name for what kind of header this is.")) (list :HEADER 12 (list :TEXT "This is a duodenary header."))))))


  (assert 
   (equal 
    (test-file "08_crazy_header.txt")
    (list :DOCUMENT (list :BODY (list :HEADER 100 (list :TEXT "This is a very low-level header"))))))


  (assert 
   (equal 
    (test-file "09_headers_and_paragraphs.txt")
    (list :DOCUMENT
	  (list :BODY (list :HEADER 1 (list :TEXT "Header 1.1"))
		(list :PARAGRAPH
		      (list :TEXT
			    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer dapibus ultricies justo, ut convallis sapien condimentum vel. Praesent interdum, ipsum vitae luctus placerat, turpis risus ornare tellus, eget dictum elit libero sit amet enim. Phasellus malesuada libero at sapien bibendum pretium. Sed ut ligula nunc, consectetur sodales urna. Integer nibh arcu, consectetur id dapibus a, blandit a urna. Curabitur tincidunt vulputate tellus, quis faucibus leo vestibulum ut. Phasellus luctus sem vel turpis pharetra in consequat tortor tincidunt. Duis feugiat nibh eu odio malesuada et eleifend sem sodales. Quisque tempus lobortis condimentum. Pellentesque tempus leo at ante sodales a molestie metus feugiat. Fusce et nisi non mauris pretium euismod. Proin nisl turpis, ornare id molestie tempor, tincidunt varius ligula. Sed at commodo ligula. Aliquam congue aliquam aliquet. Nam bibendum pretium lectus nec interdum. Curabitur a quam id velit lobortis rutrum ac nec massa. Aliquam sit amet risus ligula, at euismod sem. Phasellus odio mi, pretium nec condimentum in, fermentum convallis mi."))
		(list :HEADER 2 (list :TEXT "Header 2.1"))
		(list :PARAGRAPH
		      (list :TEXT
			    "Phasellus at quam arcu, a scelerisque lectus. Quisque magna sem, lobortis vel vulputate ut, ultricies quis urna. Aenean nec lectus nisl. Quisque non sem egestas massa posuere venenatis auctor quis lacus. Donec consequat, velit a scelerisque convallis, diam velit rhoncus felis, sit amet ornare felis felis non sapien. Aliquam erat volutpat. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Sed hendrerit lacinia lectus, et accumsan ante facilisis et. Fusce in mauris ligula, eget placerat odio. Donec aliquet consequat diam, venenatis viverra nulla suscipit vel."))
		(list :HEADER 2 (list :TEXT "Header 2.2"))
		(list :PARAGRAPH
		      (list :TEXT
			    "Aenean non mauris vel dui pulvinar molestie. Mauris mauris tortor, scelerisque vitae sodales at, egestas ac dolor. Suspendisse potenti. Nunc mollis erat ut lectus interdum tincidunt. Donec diam eros, viverra sit amet posuere et, congue quis arcu. Vestibulum ac odio id nulla posuere adipiscing in quis arcu. Phasellus sed magna sed erat hendrerit consequat. Vivamus eget nisi eget neque hendrerit rhoncus eget quis felis. Ut ut nibh nisi. Phasellus vehicula consequat dui a condimentum. Vestibulum pulvinar sollicitudin tortor at euismod. Donec eget velit felis. Curabitur tempor tristique augue, vel eleifend mi posuere et."))
		(list :HEADER 1 (list :TEXT "Header 1.2"))
		(list :PARAGRAPH
		      (list :TEXT
			    "Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nunc imperdiet aliquet quam, in sagittis leo accumsan eu. Cras ut lectus risus. In iaculis laoreet eros, a ultricies justo pharetra ac. Suspendisse egestas consequat mauris id porta. Suspendisse euismod luctus arcu vel accumsan. Ut id neque consequat mauris aliquet accumsan eget sed justo. Vestibulum lobortis interdum enim, vitae ultrices massa aliquam at. Maecenas eu purus egestas metus aliquet mollis a ac quam. Praesent vitae tellus eu sapien fringilla venenatis quis sit amet libero. Maecenas id malesuada dui. Suspendisse porta aliquet nisi. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos."))
		(list :PARAGRAPH
		      (list :TEXT
			    "Mauris vitae dignissim felis. Proin interdum ullamcorper turpis malesuada euismod. Nam sit amet enim dui, nec congue nibh. Maecenas sapien sem, imperdiet id convallis in, gravida at urna. Curabitur interdum scelerisque libero, semper tempor metus pharetra a. Mauris eu lorem eget arcu scelerisque sollicitudin sit amet eu libero. Sed auctor sagittis libero ac sagittis. Vestibulum a tellus ante, quis eleifend sem. Suspendisse imperdiet velit at justo fermentum vel facilisis nibh tempus. Suspendisse porta, ipsum non aliquet consequat, orci purus viverra nisi, in suscipit elit nulla at neque. Aenean lobortis, est sed rutrum dapibus, mauris nulla suscipit nulla, vitae dictum leo ligula ut nisi."))))))

  (assert 
   (equal 
    (test-file "10_blockquote.txt")
    (list :DOCUMENT (list :BODY (list :QUOTE (list :PARAGRAPH (list :TEXT "This is a blockquote paragraph")))))))

  (assert 
   (equal 
    (test-file "11_multiline_blockquote.txt")
    (list :DOCUMENT (list :BODY (list :QUOTE (list :PARAGRAPH (list :TEXT "This is a blockquote that spans multiple lines.")))))))

  (assert 
   (equal 
    (test-file "12_multi_paragraph_blockquote.txt")
    (list :DOCUMENT
	  (list :BODY
		(list :QUOTE 
		      (list :PARAGRAPH (list :TEXT "This is a blockquote.")) 
		      (list :PARAGRAPH (list :TEXT "This is a second paragraph in a blockquote."))
		      (list :PARAGRAPH (list :TEXT "This is a third paragraph in the blockquote.")))))))

  (assert 
   (equal 
    (test-file "13_paragraphs_and_blockquotes.txt")
    (list :DOCUMENT
	  (list :BODY
		(list :PARAGRAPH
		      (list :TEXT "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer dapibus ultricies justo, ut convallis sapien condimentum vel. Praesent interdum, ipsum vitae luctus placerat, turpis risus ornare tellus, eget dictum elit libero sit amet enim. Phasellus malesuada libero at sapien bibendum pretium. Sed ut ligula nunc, consectetur sodales urna. Integer nibh arcu, consectetur id dapibus a, blandit a urna. Curabitur tincidunt vulputate tellus, quis faucibus leo vestibulum ut. Phasellus luctus sem vel turpis pharetra in consequat tortor tincidunt. Duis feugiat nibh eu odio malesuada et eleifend sem sodales. Quisque tempus lobortis condimentum. Pellentesque tempus leo at ante sodales a molestie metus feugiat. Fusce et nisi non mauris pretium euismod. Proin nisl turpis, ornare id molestie tempor, tincidunt varius ligula. Sed at commodo ligula. Aliquam congue aliquam aliquet. Nam bibendum pretium lectus nec interdum. Curabitur a quam id velit lobortis rutrum ac nec massa. Aliquam sit amet risus ligula, at euismod sem. Phasellus odio mi, pretium nec condimentum in, fermentum convallis mi."))
		(list :QUOTE
		      (list :PARAGRAPH
			    (list :TEXT "Phasellus at quam arcu, a scelerisque lectus. Quisque magna sem, lobortis vel vulputate ut, ultricies quis urna. Aenean nec lectus nisl. Quisque non sem egestas massa posuere venenatis auctor quis lacus. Donec consequat, velit a scelerisque convallis, diam velit rhoncus felis, sit amet ornare felis felis non sapien. Aliquam erat volutpat. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Sed hendrerit lacinia lectus, et accumsan ante facilisis et. Fusce in mauris ligula, eget placerat odio. Donec aliquet consequat diam, venenatis viverra nulla suscipit vel.")))
		(list :PARAGRAPH
		      (list :TEXT "Aenean non mauris vel dui pulvinar molestie. Mauris mauris tortor, scelerisque vitae sodales at, egestas ac dolor. Suspendisse potenti. Nunc mollis erat ut lectus interdum tincidunt. Donec diam eros, viverra sit amet posuere et, congue quis arcu. Vestibulum ac odio id nulla posuere adipiscing in quis arcu. Phasellus sed magna sed erat hendrerit consequat. Vivamus eget nisi eget neque hendrerit rhoncus eget quis felis. Ut ut nibh nisi. Phasellus vehicula consequat dui a condimentum. Vestibulum pulvinar sollicitudin tortor at euismod. Donec eget velit felis. Curabitur tempor tristique augue, vel eleifend mi posuere et."))
		(list :QUOTE
		      (list :PARAGRAPH
			    (list :TEXT "Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nunc imperdiet aliquet quam, in sagittis leo accumsan eu. Cras ut lectus risus. In iaculis laoreet eros, a ultricies justo pharetra ac. Suspendisse egestas consequat mauris id porta. Suspendisse euismod luctus arcu vel accumsan. Ut id neque consequat mauris aliquet accumsan eget sed justo. Vestibulum lobortis interdum enim, vitae ultrices massa aliquam at. Maecenas eu purus egestas metus aliquet mollis a ac quam. Praesent vitae tellus eu sapien fringilla venenatis quis sit amet libero. Maecenas id malesuada dui. Suspendisse porta aliquet nisi. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos."))
		      (list :PARAGRAPH
			    (list :TEXT "Mauris vitae dignissim felis. Proin interdum ullamcorper turpis malesuada euismod. Nam sit amet enim dui, nec congue nibh. Maecenas sapien sem, imperdiet id convallis in, gravida at urna. Curabitur interdum scelerisque libero, semper tempor metus pharetra a. Mauris eu lorem eget arcu scelerisque sollicitudin sit amet eu libero. Sed auctor sagittis libero ac sagittis. Vestibulum a tellus ante, quis eleifend sem. Suspendisse imperdiet velit at justo fermentum vel facilisis nibh tempus. Suspendisse porta, ipsum non aliquet consequat, orci purus viverra nisi, in suscipit elit nulla at neque. Aenean lobortis, est sed rutrum dapibus, mauris nulla suscipit nulla, vitae dictum leo ligula ut nisi.")))
		(list :PARAGRAPH
		      (list :TEXT "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec sodales cursus sagittis. Proin eu mattis tortor. Duis egestas viverra ornare. Vivamus convallis nisi a enim luctus auctor. Sed rutrum facilisis risus dapibus sagittis. Proin id ligula ipsum, vitae fermentum enim. Mauris rutrum, ante a lobortis tempus, libero nulla imperdiet enim, vel accumsan sapien nulla sed sem. Nunc scelerisque, odio quis feugiat faucibus, nisl nisl adipiscing ligula, a ullamcorper sapien metus in sem. Ut tempor nisi vel eros rhoncus eu vulputate dui tempor. Cras nec diam neque. Nullam ac est et est tincidunt suscipit. Proin vitae velit vitae ante accumsan tristique. Donec justo nulla, consequat nec vestibulum a, molestie quis magna. Nulla vitae placerat neque. Proin laoreet, mauris sed accumsan ornare, ipsum elit feugiat mi, in fringilla dolor leo at felis. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae;"))))))

  (assert 
   (equal 
    (test-file "14_simple_verbatim.txt")
    (list :DOCUMENT (list :BODY (list :VERBATIM "This is simple verbatim text.")))))

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
