(defun read-lines (filename)
  (with-open-file (file filename)
    (loop as line = (read-line file nil)
          while line
          collect line)))

;; http://wobh.github.io/cl-cookbook/strings.html#reverse
(defun split-string (string)
  (loop for i = 0 then (1+ j)
        as j = (position #\space string :start i)
        collect (subseq string i j)
        while j))
