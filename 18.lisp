(defun parse-input (filename)
  (let ((voxels (make-hash-table :test #'equal)))
    (with-open-file (file filename)
      (loop
        (handler-case
          (let ((line (read-line file)))
            (dotimes (i (length line))
              (when (equal (char line i) #\,)
                (setf (char line i) #\space)))
            (setf (gethash (read-from-string (concatenate 'string "(" line ")")) voxels) t))
          (end-of-file () (return)))))
    voxels))

(defun find-exterior (min-x max-x min-y max-y min-z max-z voxels)
  (let ((stack nil)
         (visited (make-hash-table :test #'equal)))
    (push (list min-x min-y min-z) stack)
    (loop while stack
          do (let* ((voxel (pop stack))
                    (x (car voxel))
                    (y (cadr voxel))
                    (z (caddr voxel)))
               (when (and (<= min-x x max-x) (<= min-y y max-y) (<= min-z z max-z) (not (gethash voxel visited)))
                 (setf (gethash voxel visited) t)
                 (unless (gethash voxel voxels)
                   (push (list (1- x) y z) stack)
                   (push (list (1+ x) y z) stack)
                   (push (list x (1- y) z) stack)
                   (push (list x (1+ y) z) stack)
                   (push (list x y (1- z)) stack)
                   (push (list x y (1+ z)) stack)))))
    (loop for x from min-x to max-x
          do (loop for y from min-y to max-y
                   do (loop for z from min-z to max-z
                            do (unless (gethash (list x y z) visited)
                                 (setf (gethash (list x y z) voxels) t)))))))

(defun find-surface-area (voxels)
  (let ((count 0))
    (maphash #'(lambda (voxel _)
                 (let ((result 6)
                       (x (car voxel))
                       (y (cadr voxel))
                       (z (caddr voxel)))
                   (when (gethash (list (1- x) y z) voxels)
                     (decf result))
                   (when (gethash (list (1+ x) y z) voxels)
                     (decf result))
                   (when (gethash (list x (1- y) z) voxels)
                     (decf result))
                   (when (gethash (list x (1+ y) z) voxels)
                     (decf result))
                   (when (gethash (list x y (1- z)) voxels)
                     (decf result))
                   (when (gethash (list x y (1+ z)) voxels)
                     (decf result))
                   (setf count (+ count result)))) voxels)
    count))

(let ((voxels (parse-input "input"))
      (min-x 99999)
      (max-x -99999)
      (min-y 99999)
      (max-y -99999)
      (min-z 99999)
      (max-z -99999))
  (maphash #'(lambda (voxel _)
               (let ((x (car voxel))
                     (y (cadr voxel))
                     (z (caddr voxel)))
                 (when (< x min-x) (setf min-x x))
                 (when (> x max-x) (setf max-x x))
                 (when (< y min-y) (setf min-y y))
                 (when (> y max-y) (setf max-y y))
                 (when (< z min-z) (setf min-z z))
                 (when (> z max-z) (setf max-z z)))) voxels)
  (format t "part 1: ~a~%" (find-surface-area voxels))
  (decf min-x)
  (incf max-x)
  (decf min-y)
  (incf max-y)
  (decf min-z)
  (incf max-z)
  (find-exterior min-x max-x min-y max-y min-z max-z voxels)
  (format t "part 2: ~a~%" (find-surface-area voxels)))