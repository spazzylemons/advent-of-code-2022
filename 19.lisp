(ql:quickload "bordeaux-threads")

(declaim (optimize speed))

(defstruct blueprint
  index
  ore-robot-cost-ore
  clay-robot-cost-ore
  obs-robot-cost-ore
  obs-robot-cost-clay
  geode-robot-cost-ore
  geode-robot-cost-obs)

(defparameter *blueprint-args* (list
  ':index
  ':ore-robot-cost-ore
  ':clay-robot-cost-ore
  ':obs-robot-cost-ore
  ':obs-robot-cost-clay
  ':geode-robot-cost-ore
  ':geode-robot-cost-obs))

(defun parse-line (line)
  (dotimes (i (length line))
    (unless (<= 48 (char-code (char line i)) 57)
      (setf (char line i) #\space)))
  (let ((args nil) (arg *blueprint-args*))
    (dolist (a (read-from-string (concatenate 'string "(" line ")")))
      (push (car arg) args)
      (push a args)
      (setf arg (cdr arg)))
    (setf args (nreverse args))
    (apply #'make-blueprint args)))

(defun parse-input (filename)
  (let ((blueprints nil))
    (with-open-file (file filename)
      (loop
        (handler-case
          (push (parse-line (read-line file)) blueprints)
          (end-of-file () (return)))))
    blueprints))

(defparameter *minutes* 24)

(defun maximize-recursive (cache blueprint minute ore clay obs ore-robot clay-robot obs-robot geode-robot)
  (let* ((key (list minute ore clay obs ore-robot clay-robot obs-robot geode-robot))
         (cached (gethash key cache)))
    (or cached
        (let ((best-result 0)
              (max-ore-cost (max (blueprint-ore-robot-cost-ore blueprint)
                                 (blueprint-clay-robot-cost-ore blueprint)
                                 (blueprint-obs-robot-cost-ore blueprint)
                                 (blueprint-geode-robot-cost-ore blueprint))))
          (when (< minute *minutes*)
            (cond ((and (>= ore (blueprint-geode-robot-cost-ore blueprint))
                        (>= obs (blueprint-geode-robot-cost-obs blueprint)))
                   (setf best-result (max best-result (maximize-recursive cache
                                                                          blueprint
                                                                          (1+ minute)
                                                                          (+ ore-robot (- ore (blueprint-geode-robot-cost-ore blueprint)))
                                                                          (+ clay-robot clay)
                                                                          (+ obs-robot (- obs (blueprint-geode-robot-cost-obs blueprint)))
                                                                          ore-robot
                                                                          clay-robot
                                                                          obs-robot
                                                                          (1+ geode-robot)))))
                  ((and (>= ore max-ore-cost)
                        (>= clay (blueprint-obs-robot-cost-clay blueprint))
                        (< obs-robot (blueprint-geode-robot-cost-obs blueprint)))
                   (setf best-result (max best-result (maximize-recursive cache
                                                                          blueprint
                                                                          (1+ minute)
                                                                          (+ ore-robot (- ore (blueprint-obs-robot-cost-ore blueprint)))
                                                                          (+ clay-robot (- clay (blueprint-obs-robot-cost-clay blueprint)))
                                                                          (+ obs-robot obs)
                                                                          ore-robot
                                                                          clay-robot
                                                                          (1+ obs-robot)
                                                                          geode-robot))))
                  (t
                   (when (and (>= ore (blueprint-ore-robot-cost-ore blueprint)) (< ore-robot max-ore-cost))
                     (setf best-result (max best-result (maximize-recursive cache
                                                                            blueprint
                                                                            (1+ minute)
                                                                            (+ ore-robot (- ore (blueprint-ore-robot-cost-ore blueprint)))
                                                                            (+ clay-robot clay)
                                                                            (+ obs-robot obs)
                                                                            (1+ ore-robot)
                                                                            clay-robot
                                                                            obs-robot
                                                                            geode-robot))))
                   (when (and (>= ore (blueprint-clay-robot-cost-ore blueprint)) (< clay-robot (blueprint-obs-robot-cost-clay blueprint)))
                     (setf best-result (max best-result (maximize-recursive cache
                                                                            blueprint
                                                                            (1+ minute)
                                                                            (+ ore-robot (- ore (blueprint-clay-robot-cost-ore blueprint)))
                                                                            (+ clay-robot clay)
                                                                            (+ obs-robot obs)
                                                                            ore-robot
                                                                            (1+ clay-robot)
                                                                            obs-robot
                                                                            geode-robot))))
                   (when (and (>= ore (blueprint-obs-robot-cost-ore blueprint))
                              (>= clay (blueprint-obs-robot-cost-clay blueprint))
                              (< obs-robot (blueprint-geode-robot-cost-obs blueprint)))
                     (setf best-result (max best-result (maximize-recursive cache
                                                                            blueprint
                                                                            (1+ minute)
                                                                            (+ ore-robot (- ore (blueprint-obs-robot-cost-ore blueprint)))
                                                                            (+ clay-robot (- clay (blueprint-obs-robot-cost-clay blueprint)))
                                                                            (+ obs-robot obs)
                                                                            ore-robot
                                                                            clay-robot
                                                                            (1+ obs-robot)
                                                                            geode-robot))))
                   (setf best-result (max best-result (maximize-recursive cache
                                                                          blueprint
                                                                          (1+ minute)
                                                                          (+ ore-robot ore)
                                                                          (+ clay-robot clay)
                                                                          (+ obs-robot obs)
                                                                          ore-robot
                                                                          clay-robot
                                                                          obs-robot
                                                                          geode-robot))))))
          (setf best-result (+ best-result geode-robot))
          (setf (gethash key cache) best-result)
          best-result))))

(defun maximize (blueprint)
  (let ((cache (make-hash-table :test #'equal)))
    (maximize-recursive cache blueprint 1 0 0 0 1 0 0 0)))

(let ((blueprints (parse-input "input"))
      (result 0)
      (result-lock (bt:make-lock nil))
      (threads nil))
  (setf blueprints (nreverse blueprints))
  (dolist (blueprint blueprints)
    (let ((thread (bt:make-thread #'(lambda ()
                                      (let ((geodes (maximize blueprint)))
                                        (bt:with-lock-held (result-lock)
                                          (setf result (+ result (* (blueprint-index blueprint) geodes)))))))))
      (push thread threads)))
  (dolist (thread threads)
    (bt:join-thread thread))
  (format t "part 1: ~a~%" result)
  (setf threads nil)
  (setf result 1)
  (setf *minutes* 32)
  (dotimes (_ 3)
    (let* ((blueprint (pop blueprints))
           (thread (bt:make-thread #'(lambda ()
                                       (let ((geodes (maximize blueprint)))
                                         (bt:with-lock-held (result-lock)
                                           (setf result (* result geodes))))))))
      (push thread threads)))
  (dolist (thread threads)
    (bt:join-thread thread))
  (format t "part 2: ~a~%" result))
