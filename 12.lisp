(load "util.lisp")

(defun load-grid (filename)
  (let* ((lines (read-lines filename))
         (grid (make-array (list (length lines) (length (car lines)))))
         (y 0)
         (start-pos nil)
         (goal-pos nil))
    (dolist (line lines)
      (dotimes (x (length line))
        (let* ((c (char line x))
               (height (case c (#\S (setf start-pos (cons y x)) 0)
                               (#\E (setf goal-pos (cons y x)) 25)
                               (otherwise (- (char-code c) 97)))))
          (setf (aref grid y x) height)))
      (incf y))
    (values grid start-pos goal-pos)))

(defun hill-climb (grid start-pos goal-pos any-position)
  (let* ((dist (make-array (array-dimensions grid)))
         (grid-height (array-dimension grid 0))
         (grid-width (array-dimension grid 1))
         (queue nil)
         (sort-fn #'(lambda (a b)
           (< (aref dist (car a) (cdr a)) (aref dist (car b) (cdr b))))))
    (setf (aref dist (car start-pos) (cdr start-pos)) 0)
    (dotimes (y grid-height)
      (dotimes (x grid-width)
        (if (cond (any-position (= (aref grid y x) 0))
                  (t (equal (cons y x) start-pos)))
          (setf (aref dist y x) 0)
          (setf (aref dist y x) 100000000))
        (push (cons y x) queue)))
    (setf queue (sort queue sort-fn))
    (loop while queue
          do (let* ((u (pop queue))
                    (uy (car u))
                    (ux (cdr u))
                    (neighbors nil)
                    (needs-sorting nil))
               (when (< (1+ uy) grid-height) (push (cons (1+ uy) ux) neighbors))
               (when (< (1+ ux) grid-width) (push (cons uy (1+ ux)) neighbors))
               (when (> uy 0) (push (cons (1- uy) ux) neighbors))
               (when (> ux 0) (push (cons uy (1- ux)) neighbors))
               (dolist (v neighbors)
                 (let ((vy (car v))
                       (vx (cdr v))
                       (alt (1+ (aref dist uy ux))))
                   (when (and (< (- (aref grid vy vx) (aref grid uy ux)) 2) (< alt (aref dist vy vx))
                     (setf (aref dist vy vx) alt)
                     (setf needs-sorting t)))))
               (when needs-sorting
                 (setf queue (sort queue sort-fn)))))
  (aref dist (car goal-pos) (cdr goal-pos))))

(multiple-value-bind (grid start-pos goal-pos) (load-grid "input")
  (format t "part 1: ~a~%" (hill-climb grid start-pos goal-pos nil))
  (format t "part 2: ~a~%" (hill-climb grid start-pos goal-pos t)))
