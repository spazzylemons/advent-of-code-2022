(defun read-input (filename)
  (let ((grid (make-hash-table :test #'equal))
        (y 0))
    (with-open-file (file filename)
      (loop
        (handler-case
          (let ((line (read-line file)))
            (dotimes (x (length line))
              (when (equal (char line x) #\#)
                (setf (gethash (cons y x) grid) t)))
            (incf y))
          (end-of-file () (return)))))
    grid))

(defun perform-iteration (grid directions)
  (let ((proposals (make-hash-table :test #'equal))
        (did-move nil))
    (maphash #'(lambda (pos _)
                 (let ((can-go-n (not (or (gethash (cons (1- (car pos)) (1- (cdr pos))) grid)
                                          (gethash (cons (1- (car pos)) (cdr pos)) grid)
                                          (gethash (cons (1- (car pos)) (1+ (cdr pos))) grid))))
                       (can-go-s (not (or (gethash (cons (1+ (car pos)) (1- (cdr pos))) grid)
                                          (gethash (cons (1+ (car pos)) (cdr pos)) grid)
                                          (gethash (cons (1+ (car pos)) (1+ (cdr pos))) grid))))
                       (can-go-w (not (or (gethash (cons (1- (car pos)) (1- (cdr pos))) grid)
                                          (gethash (cons (car pos) (1- (cdr pos))) grid)
                                          (gethash (cons (1+ (car pos)) (1- (cdr pos))) grid))))
                       (can-go-e (not (or (gethash (cons (1- (car pos)) (1+ (cdr pos))) grid)
                                          (gethash (cons (car pos) (1+ (cdr pos))) grid)
                                          (gethash (cons (1+ (car pos)) (1+ (cdr pos))) grid))))
                       (found-dir nil))
                 (unless (and can-go-n can-go-s can-go-w can-go-e)
                   (dolist (dir directions)
                     (cond (found-dir nil)
                           ((and (equal dir 'n) can-go-n)
                            (let ((proposed (cons (1- (car pos)) (cdr pos))))
                              (if (gethash proposed proposals)
                                (setf (gethash proposed proposals) 'invalid)
                                (setf (gethash proposed proposals) pos)))
                            (setf found-dir t))
                           ((and (equal dir 's) can-go-s)
                            (let ((proposed (cons (1+ (car pos)) (cdr pos))))
                              (if (gethash proposed proposals)
                                (setf (gethash proposed proposals) 'invalid)
                                (setf (gethash proposed proposals) pos)))
                            (setf found-dir t))
                           ((and (equal dir 'w) can-go-w)
                            (let ((proposed (cons (car pos) (1- (cdr pos)))))
                              (if (gethash proposed proposals)
                                (setf (gethash proposed proposals) 'invalid)
                                (setf (gethash proposed proposals) pos)))
                            (setf found-dir t))
                           ((and (equal dir 'e) can-go-e)
                            (let ((proposed (cons (car pos) (1+ (cdr pos)))))
                              (if (gethash proposed proposals)
                                (setf (gethash proposed proposals) 'invalid)
                                (setf (gethash proposed proposals) pos))
                            (setf found-dir t)))))))) grid)
    (maphash #'(lambda (dest src)
                 (when (and src (not (equal src 'invalid)))
                   (remhash src grid)
                   (setf (gethash dest grid) t)
                   (setf did-move t))) proposals)
    did-move))

(defun part-1 (grid num-iterations)
  (let ((directions (list 'n 's 'w 'e)))
    (dotimes (_ num-iterations)
      (perform-iteration grid directions)
      (setf (cdr (last directions)) (list (pop directions)))))
  (let ((min-y nil)
        (max-y nil)
        (min-x nil)
        (max-x nil))
    (maphash #'(lambda (pos _)
                 (when (or (not min-y) (< (car pos) min-y))
                   (setf min-y (car pos)))
                 (when (or (not max-y) (> (car pos) max-y))
                   (setf max-y (car pos)))
                 (when (or (not min-x) (< (cdr pos) min-x))
                   (setf min-x (cdr pos)))
                 (when (or (not max-x) (> (cdr pos) max-x))
                   (setf max-x (cdr pos)))) grid)
    (- (* (1+ (- max-x min-x)) (1+ (- max-y min-y))) (hash-table-count grid))))

(defun part-2 (grid)
  (let ((directions (list 'n 's 'w 'e))
        (num-rounds 0))
    (loop
      (incf num-rounds)
      (unless (perform-iteration grid directions)
        (return))
      (setf (cdr (last directions)) (list (pop directions))))
    num-rounds))

(let ((grid (read-input "input")))
  (format t "part 1: ~a~%" (part-1 grid 10)))

(let ((grid (read-input "input")))
  (format t "part 2: ~a~%" (part-2 grid)))
