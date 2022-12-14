(load "util.lisp")

(defun parse-stack-row (line)
  (let ((row-length (/ (1+ (length line)) 4)))
    (loop for i below row-length
          as c = (char line (1+ (* i 4)))
          collect (if (equal c #\space) nil c))))

(defun solver (do-reverse)
  (with-open-file (file "input")
    (let ((transposed-stacks (loop as line = (read-line file nil)
                                   until (= (length line) 0)
                                   collect line))
          (stacks nil))
      (setf transposed-stacks (nreverse transposed-stacks))
      (pop transposed-stacks)
      (setf transposed-stacks (mapcar #'parse-stack-row transposed-stacks))
      (setf stacks (make-array (length (car transposed-stacks)) :initial-element nil))
      (dolist (s transposed-stacks)
        (let ((i 0))
          (dolist (c s)
            (when c (push c (aref stacks i)))
            (incf i))))
      (loop as line = (read-line file nil)
            while line
            do (let* ((split (split-string line))
                      (num (parse-integer (nth 1 split)))
                      (src (1- (parse-integer (nth 3 split))))
                      (dst (1- (parse-integer (nth 5 split))))
                      (stack nil))
                 (dotimes (i num)
                   (push (pop (aref stacks src)) stack))
                 (when do-reverse (setf stack (nreverse stack)))
                 (dotimes (i num)
                   (push (pop stack) (aref stacks dst)))))
      (dotimes (i (length stacks))
        (format t "~a" (pop (aref stacks i))))
      (format t "~%"))))

(format t "part 1: ")
(solver t)
(format t "part 2: ")
(solver nil)
