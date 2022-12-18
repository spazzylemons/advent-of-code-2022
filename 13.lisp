(defun parse-packet (line)
  ; convert to lisp syntax
  (dotimes (i (length line))
    (case (char line i)
      (#\[ (setf (char line i) #\())
      (#\] (setf (char line i) #\)))
      (#\, (setf (char line i) #\space))))
  (read-from-string line))

(define-condition in-order () (simple-condition))
(define-condition out-of-order () (simple-condition))

(defun compare-packets (packet-1 packet-2)
  (cond
    ((and (integerp packet-1) (integerp packet-2))
     (cond ((< packet-1 packet-2) (signal 'in-order))
           ((> packet-1 packet-2) (signal 'out-of-order))))
    ((and (listp packet-1) (integerp packet-2))
     (when (not packet-1)
      (signal 'in-order))
     (compare-packets packet-1 (list packet-2)))
    ((and (integerp packet-1) (listp packet-2))
     (when (not packet-2)
      (signal 'out-of-order))
     (compare-packets (list packet-1) packet-2))
    ((and (listp packet-1) (listp packet-2))
     (loop while (or packet-1 packet-2)
          do (cond ((and packet-1 (not packet-2))
                    (signal 'out-of-order))
                   ((and (not packet-1) packet-2)
                    (signal 'in-order)))
             (compare-packets (car packet-1) (car packet-2))
             (setf packet-1 (cdr packet-1))
             (setf packet-2 (cdr packet-2))))))

(defun sort-packets (a b)
  (handler-case
    (compare-packets a b)
    (in-order () t)
    (out-of-order () nil)))

(let ((part-1-result 0)
      (i 1)
      (two-index nil)
      (six-index nil)
      (packets (list '((2)) '((6)))))
  (with-open-file (file "input")
    (loop
      (handler-case
        (let ((packet-1 (parse-packet (read-line file)))
              (packet-2 (parse-packet (read-line file))))
          (push packet-1 packets)
          (push packet-2 packets)
          (compare-packets packet-1 packet-2))
        (in-order () (setf part-1-result (+ part-1-result i)))
        (out-of-order ()))
      (handler-case
        (read-line file)
        (end-of-file () (return)))
      (incf i)))
  (format t "part 1: ~a~%" part-1-result)
  (setf packets (sort packets #'sort-packets))
  (setf i 1)
  (loop while (or (not two-index) (not six-index))
    do (cond ((equal (car packets) '((2)))
              (setf two-index i))
             ((equal (car packets) '((6)))
              (setf six-index i)))
       (incf i)
       (setf packets (cdr packets)))
  (format t "part 2: ~a~%" (* two-index six-index)))