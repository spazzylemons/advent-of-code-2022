(defstruct monkey name value lhs rhs op)

(defun parse-monkey (line)
  (let ((name (subseq line 0 4))
        (monkey (make-monkey)))
    (setf (monkey-name monkey) name)
    (if (= (length line) 17)
      (let ((lhs (subseq line 6 10))
            (rhs (subseq line 13 17))
            (op (char line 11)))
        (setf (monkey-lhs monkey) lhs)
        (setf (monkey-rhs monkey) rhs)
        (setf (monkey-op monkey) (case op (#\+ #'+) (#\- #'-) (#\* #'*) (#\/ #'/))))
      (setf (monkey-value monkey) (parse-integer (subseq line 5))))
    monkey))

(defun parse-input (filename)
  (let ((monkeys (make-hash-table :test #'equal)))
    (with-open-file (file filename)
      (loop
        (handler-case
          (let ((monkey (parse-monkey (read-line file))))
            (setf (gethash (monkey-name monkey) monkeys) monkey))
          (end-of-file () (return)))))
    ; connect the monkeys
    (maphash #'(lambda (name monkey)
                 (when (not (monkey-value monkey))
                   (setf (monkey-lhs monkey) (gethash (monkey-lhs monkey) monkeys))
                   (setf (monkey-rhs monkey) (gethash (monkey-rhs monkey) monkeys))))
      monkeys)
    monkeys))

(defun simplify (monkey)
  (if (and (not (monkey-value monkey)) (not (equal (monkey-name monkey) "humn")))
    (let* ((monkey-a (simplify (monkey-lhs monkey)))
           (monkey-b (simplify (monkey-rhs monkey)))
           (value-a (monkey-value monkey-a))
           (value-b (monkey-value monkey-b)))
      (cond ((and value-a value-b)
             (setf (monkey-value monkey) (apply (monkey-op monkey) (list value-a value-b)))))
      monkey)
    monkey))

(defun part-2 (monkey)
  (let ((lhs (monkey-lhs monkey))
        (rhs (monkey-rhs monkey)))
    (loop
      (setf lhs (simplify lhs))
      (setf rhs (simplify rhs))
      (cond ((and (equal (monkey-name lhs) "humn") (monkey-value rhs))
             (return (monkey-value rhs)))
            ((and (equal (monkey-op lhs) #'+)
                  (monkey-value (monkey-lhs lhs))
                  (not (monkey-value (monkey-rhs lhs))))
             (setf rhs (make-monkey :lhs rhs :rhs (monkey-lhs lhs) :op #'-))
             (setf lhs (monkey-rhs lhs)))
            ((and (equal (monkey-op lhs) #'+)
                  (not (monkey-value (monkey-lhs lhs)))
                  (monkey-value (monkey-rhs lhs)))
             (setf rhs (make-monkey :lhs rhs :rhs (monkey-rhs lhs) :op #'-))
             (setf lhs (monkey-lhs lhs)))
            ((and (equal (monkey-op lhs) #'-)
                  (monkey-value (monkey-lhs lhs))
                  (not (monkey-value (monkey-rhs lhs))))
             (setf (monkey-rhs lhs) (make-monkey :lhs (monkey-rhs lhs) :rhs (make-monkey :value -1) :op #'*))
             (setf (monkey-op lhs) #'+))
            ((and (equal (monkey-op lhs) #'-)
                  (not (monkey-value (monkey-lhs lhs)))
                  (monkey-value (monkey-rhs lhs)))
             (setf rhs (make-monkey :lhs rhs :rhs (monkey-rhs lhs) :op #'+))
             (setf lhs (monkey-lhs lhs)))
            ((and (equal (monkey-op lhs) #'*)
                  (monkey-value (monkey-lhs lhs))
                  (not (monkey-value (monkey-rhs lhs))))
             (setf rhs (make-monkey :lhs rhs :rhs (monkey-lhs lhs) :op #'/))
             (setf lhs (monkey-rhs lhs)))
            ((and (equal (monkey-op lhs) #'*)
                  (not (monkey-value (monkey-lhs lhs)))
                  (monkey-value (monkey-rhs lhs)))
             (setf rhs (make-monkey :lhs rhs :rhs (monkey-rhs lhs) :op #'/))
             (setf lhs (monkey-lhs lhs)))
            ((and (equal (monkey-op lhs) #'/)
                  (not (monkey-value (monkey-lhs lhs)))
                  (monkey-value (monkey-rhs lhs)))
             (setf rhs (make-monkey :lhs rhs :rhs (monkey-rhs lhs) :op #'*))
             (setf lhs (monkey-lhs lhs)))))))

(let ((monkeys (parse-input "input")))
  (format t "part 1: ~a~%" (monkey-value (simplify (gethash "root" monkeys)))))

(let ((monkeys (parse-input "input")))
  (setf (monkey-value (gethash "humn" monkeys)) nil)
  (format t "part 2: ~a~%" (part-2 (gethash "root" monkeys))))
