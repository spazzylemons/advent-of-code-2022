(load "util.lisp")

(defun move-rope (head tail)
  (let ((dx (abs (- (car head) (car tail))))
        (dy (abs (- (cdr head) (cdr tail)))))
          (cond ((and (< dx 2) (< dy 2))
                   nil)
                ((and (= dx 0) (= dy 2))
                  (if (< (cdr head) (cdr tail)) (decf (cdr tail)) (incf (cdr tail))))
                ((and (= dx 2) (= dy 0))
                  (if (< (car head) (car tail)) (decf (car tail)) (incf (car tail))))
                (t
                  (if (< (car head) (car tail)) (decf (car tail)) (incf (car tail)))
                  (if (< (cdr head) (cdr tail)) (decf (cdr tail)) (incf (cdr tail))))))
  tail)

(defun simulate-ropes (node-count)
  (let ((nodes nil)
        (tail nil)
        (visited (make-hash-table :test #'equal)))
    (dotimes (_ node-count)
      (push (cons 0 0) nodes))
    (setf (gethash (cons 0 0) visited) t)
    (dolist (line (read-lines "input"))
      (let ((direction (char line 0))
            (count (parse-integer (subseq line 2))))
        (dotimes (_ count)
          (case direction
            (#\U (decf (cdar nodes)))
            (#\D (incf (cdar nodes)))
            (#\L (decf (caar nodes)))
            (#\R (incf (caar nodes))))
          (let ((iter nodes))
            (loop while (cdr iter)
                  do (setf (cadr iter) (move-rope (car iter) (cadr iter)))
                     (setf iter (cdr iter)))
            (setf (gethash (copy-tree (car iter)) visited) t)))))
    (hash-table-count visited)))

(format t "part 1: ~a~%" (simulate-ropes 2))
(format t "part 2: ~a~%" (simulate-ropes 10))
