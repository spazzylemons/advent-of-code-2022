(defparameter *test-y* 2000000)
(defparameter *search-space* 4000000)

(defun parse-line (line)
  (let* ((sensor-x-start (1+ (position #\= line)))
         (sensor-x-end (position #\, line :start sensor-x-start))
         (sensor-y-start (1+ (position #\= line :start sensor-x-start)))
         (sensor-y-end (position #\: line :start sensor-y-start))
         (beacon-x-start (1+ (position #\= line :start sensor-y-end)))
         (beacon-x-end (position #\, line :start beacon-x-start))
         (beacon-y-start (1+ (position #\= line :start beacon-x-end)))
         (sensor-x (parse-integer (subseq line sensor-x-start sensor-x-end)))
         (sensor-y (parse-integer (subseq line sensor-y-start sensor-y-end)))
         (beacon-x (parse-integer (subseq line beacon-x-start beacon-x-end)))
         (beacon-y (parse-integer (subseq line beacon-y-start)))
         (distance (+ (abs (- sensor-x beacon-x)) (abs (- sensor-y beacon-y)))))
    ; store sensor as combo of position and manhattan distance
    (values (cons (cons sensor-y sensor-x) distance) (cons beacon-y beacon-x))))

(defun parse-input (filename)
  (let ((sensors nil) (beacons nil) (seen-beacons (make-hash-table :test #'equal)))
    (with-open-file (file filename)
      (loop
        (handler-case
          (multiple-value-bind (sensor beacon) (parse-line (read-line file))
            (push sensor sensors)
            (when (and (= (car beacon) *test-y*) (not (gethash (car beacon) seen-beacons)))
              (push (cdr beacon) beacons)
              (setf (gethash (car beacon) seen-beacons) t)))
          (end-of-file () (return))))
    (values sensors beacons))))

(defun test-beacons (beacons x)
  (cond ((not beacons) nil)
        ((= x (car beacons)) t)
        (t (test-beacons (cdr beacons) x))))

(defun test-ranges (ranges x)
  (cond ((not ranges) nil)
        ((<= (caar ranges) x (cdar ranges)) t)
        (t (test-ranges (cdr ranges) x))))

(define-condition obscured-beacon (simple-condition)
  ((tuning-freq
    :initarg :tuning-freq
    :accessor obscured-beacon-tuning-freq
    :initform nil)))

(defun test-mergeability (range-a range-b)
  (or (= (1+ (cdr range-a)) (car range-b))
      (<= (car range-a) (car range-b) (cdr range-a))
      (<= (car range-a) (cdr range-b) (cdr range-a))))

(defun make-merged-range (range-a range-b)
  (let ((new-min (min (car range-a) (cdr range-a) (car range-b) (cdr range-b)))
        (new-max (max (car range-a) (cdr range-a) (car range-b) (cdr range-b))))
    (cons new-min new-max)))

(defun try-merge-range (range-a range-b)
  (when (or (test-mergeability range-a range-b) (test-mergeability range-b range-a))
    (make-merged-range range-a range-b)))

(define-condition merge-performed () (simple-condition))

(defun find-missing-point (points)
  (if (= (- (cadr points) (car points)) 2) (1+ (car points)) (find-missing-point (cdr points))))

(defun merge-ranges (ranges)
  (let ((was-merge-performed t))
    (loop while was-merge-performed
          do (setf was-merge-performed nil)
             (handler-case
               (dolist (a ranges)
                 (dolist (b ranges)
                   (unless (equal a b)
                     (let ((merged (try-merge-range a b)))
                       (when merged
                         (setf ranges (delete a ranges))
                         (setf ranges (delete b ranges))
                         (push merged ranges)
                         (signal 'merge-performed))))))
               (merge-performed () (setf was-merge-performed t)))))
  (when (> (length ranges) 1)
    (let ((points (list (caar ranges) (cdar ranges) (caadr ranges) (cdadr ranges))))
      (setf points (sort points #'<))
      (find-missing-point points))))

(defun solve-part-two (sensors)
  (dotimes (test-y (1+ *search-space*))
    (let ((ranges nil) (min-range nil) (max-range nil))
      (dolist (sensor sensors)
          (let* ((pos (car sensor))
                 (search-dist (cdr sensor))
                 (sensor-dist (abs (- (car pos) test-y)))
                 (range-size (- search-dist sensor-dist)))
            (when (>= range-size 0)
              (push (cons (- (cdr pos) range-size) (+ (cdr pos) range-size)) ranges))))
      (let ((x (merge-ranges ranges)))
        (when x
          (signal 'obscured-beacon :tuning-freq (+ (* x 4000000) test-y)))))))

(let ((ranges nil) (min-range nil) (max-range nil) (part-1-result 0))
  (multiple-value-bind (sensors beacons) (parse-input "input")
    ; find ranges for each sensor
    (dolist (sensor sensors)
      (let* ((pos (car sensor))
             (search-dist (cdr sensor))
             (sensor-dist (abs (- (car pos) *test-y*)))
             (range-size (- search-dist sensor-dist)))
        (when (>= range-size 0)
          (push (cons (- (cdr pos) range-size) (+ (cdr pos) range-size)) ranges))))
    (dolist (range ranges)
      (when (or (not min-range) (< (car range) min-range))
        (setf min-range (car range)))
      (when (or (not max-range) (> (cdr range) max-range))
        (setf max-range (cdr range))))
    (loop while (<= min-range max-range)
          do (when (and (not (test-beacons beacons min-range)) (test-ranges ranges min-range))
               (incf part-1-result))
             (incf min-range))
    (format t "~a~%" part-1-result)
    (handler-case
      (solve-part-two sensors)
      (obscured-beacon (e) (format t "~a~%" (obscured-beacon-tuning-freq e))))))
