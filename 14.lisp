(defun parse-point (str)
  (let ((index (position #\, str)))
    (cons (parse-integer (subseq str 0 index))
          (parse-integer (subseq str (1+ index))))))

(defun parse-path (line)
  (loop for i = 0 then (+ j 4)
        as j = (position #\space line :start i)
        collect (parse-point (subseq line i j))
        while j))

(defun parse-paths (filename)
  (let ((paths nil))
    (with-open-file (file filename)
      (loop
        (handler-case
          (push (parse-path (read-line file)) paths)
          (end-of-file () (return))))
    paths)))

(defmacro swapf (a b)
  (let ((swap-temp (gensym)))
    `(let ((,swap-temp ,a))
      (setf ,a ,b)
      (setf ,b ,swap-temp))))

(defun trace-path (path tilemap)
  (let ((point-a (car path))
        (point-b (cadr path)))
    (when (and point-a point-b)
      ; if x equal, vertical line, otherwise horizontal
      (cond ((= (car point-a) (car point-b))
             (let ((x (car point-a))
                   (y0 (cdr point-a))
                   (y1 (cdr point-b)))
               (when (> y0 y1) (swapf y0 y1))
               (loop while (<= y0 y1)
                     do (setf (gethash (cons y0 x) tilemap) t)
                        (incf y0))))
            (t
             (let ((y (cdr point-a))
                   (x0 (car point-a))
                   (x1 (car point-b)))
               (when (> x0 x1) (swapf x0 x1))
               (loop while (<= x0 x1)
                     do (setf (gethash (cons y x0) tilemap) t)
                        (incf x0)))))
      (trace-path (cdr path) tilemap))))

(define-condition stop-iteration () (simple-condition))

(defun drop-sand (x-pos y-pos floor-y signal-floor tilemap)
  (if (= (1+ y-pos) floor-y)
    (if signal-floor (signal 'stop-iteration) (setf (gethash (cons y-pos x-pos) tilemap) t))
    (cond
      ((gethash (cons y-pos x-pos) tilemap)
       (signal 'stop-iteration))
      ((not (gethash (cons (1+ y-pos) x-pos) tilemap))
       (drop-sand x-pos (1+ y-pos) floor-y signal-floor tilemap))
      ((not (gethash (cons (1+ y-pos) (1- x-pos)) tilemap))
       (drop-sand (1- x-pos) (1+ y-pos) floor-y signal-floor tilemap))
      ((not (gethash (cons (1+ y-pos) (1+ x-pos)) tilemap))
       (drop-sand (1+ x-pos) (1+ y-pos) floor-y signal-floor tilemap))
      (t
       (setf (gethash (cons y-pos x-pos) tilemap) t)))))

(let ((paths (parse-paths "input"))
      (floor-y nil)
      (tilemap nil)
      (sand-counter 0))
  (dolist (path paths)
    (dolist (point path)
      (let ((y (cdr point)))
        (when (or (not floor-y) (> y floor-y))
          (setf floor-y y)))))
  ; create floor
  (setf floor-y (+ floor-y 2))
  ; create the tilemap
  (setf tilemap (make-hash-table :test #'equal))
  ; trace the paths
  (dolist (path paths)
    (trace-path path tilemap))
  (loop
    (handler-case
      (drop-sand 500 0 floor-y t tilemap)
      (stop-iteration () (return)))
    (incf sand-counter))
  (format t "part 1: ~a~%" sand-counter)
  (loop
    (handler-case
      (drop-sand 500 0 floor-y nil tilemap)
      (stop-iteration () (return)))
    (incf sand-counter))
  (format t "part 2: ~a~%" sand-counter))
