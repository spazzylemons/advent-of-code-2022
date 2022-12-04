(load "util.lisp")

(defun parse-section (line)
  (let* ((dash1 (position #\- line))
         (comma (position #\, line :start dash1))
         (dash2 (position #\- line :start comma))
         (a-start (parse-integer (subseq line 0 dash1)))
         (a-end (parse-integer (subseq line (1+ dash1) comma)))
         (b-start (parse-integer (subseq line (1+ comma) dash2)))
         (b-end (parse-integer (subseq line (1+ dash2)))))
    (cons (cons a-start a-end) (cons b-start b-end))))

(defun has-complete-overlap-half (section-a section-b)
  (and (<= (car section-a) (car section-b) (cdr section-a))
       (<= (car section-a) (cdr section-b) (cdr section-a))))

(defun has-complete-overlap (line)
  (let ((section (parse-section line)))
    (if (or (has-complete-overlap-half (car section) (cdr section))
            (has-complete-overlap-half (cdr section) (car section)))
        1
        0)))

(defun has-any-overlap-half (section-a section-b)
  (or (<= (car section-a) (car section-b) (cdr section-a))
      (<= (car section-a) (cdr section-b) (cdr section-a))))

(defun has-any-overlap (line)
  (let ((section (parse-section line)))
    (if (or (has-any-overlap-half (car section) (cdr section))
            (has-any-overlap-half (cdr section) (car section)))
        1
        0)))

(let ((lines (read-lines "input")))
  (format t "part 1: ~a~%" (reduce #'+ (mapcar #'has-complete-overlap lines)))
  (format t "part 2: ~a~%" (reduce #'+ (mapcar #'has-any-overlap lines))))
