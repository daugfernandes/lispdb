(defvar *tot1* 0)
(defvar *tot2* 0)
(defvar *tot3* 0)

(defun fib1 (N)
  (progn
    (incf *tot1*)
    (cond
     ((<= n 0)
      0)
     ((= n 1)
      1)
     ((= n 2)
      1)
     ((oddp n)
      (+ (* 2 (fib1 (- n 2))) (fib1 (- n 3))))
     ((evenp n)
      (+ (* 3 (fib1 (- n 3))) (* 2 (fib1 (- n 4))))))))



(defun fibtriv (N)
  (progn
    (incf *tot2*)
    (cond
     ((= n 1)
      1)
     ((= n 2)
      1)
     (T 
      (+ (fibtriv (- n 1)) (fibtriv (- n 2)))))))

(defun ut (n)
  (setf *tot1* 0)
  (setf v1 (fib n))
  (setf *tot2* 0)
  (setf v2 (fib2 n))
  (list *tot1* *tot2* "=" v1 (= v1 v2)))
