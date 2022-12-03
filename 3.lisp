(load "util.lisp")

(defparameter *priorities* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun find-misplaced-recursive (c1 c2 i)
  (let ((c (char c1 i)))
    (if (position c c2) c (find-misplaced-recursive c1 c2 (1+ i)))))

(defun find-misplaced (line)
  (let* ((midpoint (/ (length line) 2))
         (c1 (subseq line 0 midpoint))
         (c2 (subseq line midpoint)))
    (find-misplaced-recursive c1 c2 0)))

(defun get-misplaced-priority (line)
  (1+ (position (find-misplaced line) *priorities*)))

(defun group-lines (lines n)
  (let ((buffer nil)
        (result nil)
        (size 0))
    (dolist (line lines)
      (setf buffer (append buffer (list line)))
      (incf size)
      (when (= size n)
        (setf result (append result (list buffer)))
        (setf buffer nil)
        (setf size 0)))
  result))

(defun is-in-all (group c)
  (let ((line (car group)))
    (if (not line) t (and (position c line) (is-in-all (cdr group) c)))))

(defun find-common-recursive (group i)
  (if (is-in-all group (char *priorities* i)) (1+ i) (find-common-recursive group (1+ i))))

(defun find-common-in-group (group)
  (find-common-recursive group 0))

(let ((lines (read-lines "input")))
  (format t "part 1: ~a~%" (reduce #'+ (map 'list #'get-misplaced-priority lines)))
  (format t "part 2: ~a~%" (reduce #'+ (map 'list #'find-common-in-group (group-lines lines 3)))))
