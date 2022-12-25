(defun from-snafu (snafu)
  (let ((result 0))
    (dotimes (i (length snafu))
      (setf result (* result 5))
      (setf result (+ result (case (char snafu i)
                               (#\0 0)
                               (#\1 1)
                               (#\2 2)
                               (#\- -1)
                               (#\= -2)))))
    result))

(defparameter *digits* (make-array 5 :initial-contents '("=" "-" "0" "1" "2")))

(defun to-snafu (num)
  (if (= num 0)
    "0"
    (let ((result ""))
      (loop until (= num 0)
            do (let ((digit (- (mod (+ num 2) 5) 2)))
                 (setf num (/ (- num digit) 5))
                 (setf result (concatenate 'string (aref *digits* (+ digit 2)) result))))
      result)))

(defun read-input (filename)
  (let ((numbers nil))
    (with-open-file (file filename)
      (loop
        (handler-case
          (push (from-snafu (read-line file)) numbers)
          (end-of-file () (return)))))
    numbers))

(let ((numbers (read-input "input")))
  (format t "part 1: ~a~%" (to-snafu (reduce #'+ numbers))))
