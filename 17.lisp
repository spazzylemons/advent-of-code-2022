(defun get-input ()
  (with-open-file (file "input")
    (let ((line (read-line file))
          (directions nil))
      (dotimes (i (length line))
        (push (if (equal (char line i) #\>) 1 -1) directions))
      (setf directions (nreverse directions))
      directions)))

(defparameter *rock-1*
  (make-array '(4 4) :initial-contents '(
    (t   t   t   t  )
    (nil nil nil nil)
    (nil nil nil nil)
    (nil nil nil nil))))

(defparameter *rock-2*
  (make-array '(4 4) :initial-contents '(
    (nil t   nil nil)
    (t   t   t   nil)
    (nil t   nil nil)
    (nil nil nil nil))))

(defparameter *rock-3*
  (make-array '(4 4) :initial-contents '(
    (t   t   t   nil)
    (nil nil t   nil)
    (nil nil t   nil)
    (nil nil nil nil))))

(defparameter *rock-4*
  (make-array '(4 4) :initial-contents '(
    (t   nil nil nil)
    (t   nil nil nil)
    (t   nil nil nil)
    (t   nil nil nil))))

(defparameter *rock-5*
  (make-array '(4 4) :initial-contents '(
    (t   t   nil nil)
    (t   t   nil nil)
    (nil nil nil nil)
    (nil nil nil nil))))

(defun make-rocks ()
  (let ((rocks (list *rock-1*
                     *rock-2*
                     *rock-3*
                     *rock-4*
                     *rock-5*)))
    (setf (cdr (last rocks)) rocks)
    rocks))

(defun check-collision (rock x y tile-array)
  (block collision
    (let ((height (array-dimension tile-array 0))
          (width (array-dimension tile-array 1)))
      (dotimes (h 4)
        (dotimes (w 4)
          (let ((check-y (+ y h))
                (check-x (+ x w)))
            (when (and (aref rock h w) (or (< check-x 0) (>= check-x width) (< check-y 0) (>= check-y height) (= (bit tile-array check-y check-x) 1)))
              (return-from collision t))))))
    nil))

(defun in-bounds (x y tile-array)
  (let ((height (array-dimension tile-array 0))
        (width (array-dimension tile-array 1)))
    (and (<= 0 x (1- width)) (<= 0 y (1- height)))))

(defun stamp-rock (rock x y tile-array)
  (let ((height (array-dimension tile-array 0))
        (width (array-dimension tile-array 1))
        (highest-point -1))
    (dotimes (h 4)
      (dotimes (w 4)
        (let ((check-y (+ y h))
              (check-x (+ x w)))
          (when (and (aref rock h w) (not (or (< check-x 0) (>= check-x width) (< check-y 0) (>= check-y height))))
            (setf (bit tile-array check-y check-x) 1)
            (setf highest-point (max highest-point (1+ check-y)))))))
    highest-point))

(defun find-lowest-reachable (highest-point tile-array)
  (let ((stack nil)
        (lowest-point highest-point)
        (visited-points (make-hash-table :test #'equal)))
    (push (cons highest-point 3) stack)
    (loop while stack
          do (let* ((point (pop stack))
                    (y (car point))
                    (x (cdr point)))
               (when (and (<= y highest-point) (in-bounds x y tile-array) (not (gethash point visited-points)) (= (bit tile-array y x) 0))
                 (setf lowest-point (min lowest-point y))
                 (setf (gethash point visited-points) t)
                 (push (cons (1- y) x) stack)
                 (push (cons (1+ y) x) stack)
                 (push (cons y (1- x)) stack)
                 (push (cons y (1+ x)) stack))))
    lowest-point))

(defun shift-array-down (tile-array amount highest-point)
  (let ((height (array-dimension tile-array 0))
        (width (array-dimension tile-array 1))
        (shift-src amount)
        (shift-dst 0))
    (loop while (<= shift-dst highest-point)
          do (dotimes (x width)
               (setf (bit tile-array shift-dst x) (bit tile-array shift-src x)))
             (incf shift-src)
             (incf shift-dst))))

(defun serialize-tile-array (tile-array highest-point)
  (let ((height (array-dimension tile-array 0))
        (width (array-dimension tile-array 1))
        (result 0))
    (dotimes (y highest-point)
      (dotimes (x width)
        (setf result (* result 2))
        (setf result (+ result (bit tile-array y x)))))
    result))

(defparameter *total-to-check* 1000000000000)

(let ((directions (get-input))
      (rocks (make-rocks))
      (highest-point 0)
      (accumulated-height 0)
      (tile-array (make-array '(1000 7) :element-type 'bit))
      (direction-index 0)
      (rock-index 0)
      (num-directions 0)
      (seen-states (make-hash-table :test #'equal))
      (waiting-for-copy t)
      (i 0))
  (setf num-directions (length directions))
  (setf (cdr (last directions)) directions)
  (loop while (< i *total-to-check*)
       do (let ((rock (pop rocks))
                (x 2)
                (y (+ highest-point 3)))
            (loop
              (let* ((direction (pop directions))
                     (new-x (+ x direction)))
                (setf direction-index (mod (1+ direction-index) num-directions))
                (unless (check-collision rock new-x y tile-array)
                  (setf x new-x))
                (if (check-collision rock x (1- y) tile-array)
                  (progn
                    (let ((lowest-point (find-lowest-reachable highest-point tile-array)))
                      (setf accumulated-height (+ accumulated-height lowest-point))
                      (shift-array-down tile-array lowest-point highest-point)
                      (setf highest-point (- highest-point lowest-point))
                      (setf y (- y lowest-point)))
                    (setf highest-point (max (stamp-rock rock x y tile-array) highest-point))
                    (return))
                  (decf y))))
             (setf rock-index (mod (1+ rock-index) 5))
             (incf i))
           (when waiting-for-copy
            (let* ((state-key (list rock-index direction-index highest-point (serialize-tile-array tile-array highest-point)))
                   (seen-state (gethash state-key seen-states)))
              (if seen-state
                (progn
                  (let* ((blocks-in-state (- i (car seen-state)))
                         (height-in-state (- accumulated-height (cdr seen-state)))
                         (repeat-count (floor (/ (- *total-to-check* i) blocks-in-state))))
                    (setf accumulated-height (+ accumulated-height (* repeat-count height-in-state)))
                    (setf i (+ i (* repeat-count blocks-in-state)))
                    (setf waiting-for-copy nil)))
                (progn
                  (setf (gethash state-key seen-states) (cons i accumulated-height)))))))
  (format t "~a~%" (+ highest-point accumulated-height)))
