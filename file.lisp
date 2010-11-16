;;; cat file contents line by line
;;     - filename: <self-describing>
;;     - returns:  file content

(defun s7-cat-file (filename) 
  (if (probe-file filename)
    (with-open-file (stream filename)
      (loop for line = (read-line stream nil 'foo)
          until (equal line 'foo)
          do (print line)
      )
    )
    nil ; filename not exists
  )
)


