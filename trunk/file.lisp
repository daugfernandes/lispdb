;;; cat file contents line by line
;;     - filename: <self-describing>
;;     - returns:  file content

(defun s7-cat-file (fname) 
  (if (probe-file fname)           ; complete-file name or nil
    (with-open-file (stream fname) 
      (loop for line = (read-line stream nil 'foo)
          until (equal line 'foo)
          do (print line)
      )
    )
    nil ; filename not exists
  )
)

