;;; ****************************************************************
;;; Conway's Game of Life ******************************************
;;; ****************************************************************

;;; Don't know where/when got this. --mk

(defstruct (world (:print-function 
                   (lambda (s d o)
                     (declare (ignore d))
                     (format s "#<WORLD: ~D>" (world-numdots world)))))
  size
  current
  numdots
  next
  (xmin 1000000)    ; Initialize the region to ridiculous numbers.
  (xmax -1)
  (ymin 1000000)
  (ymax -1))

(defun setnext (world i j)
  (let* ((current (world-current world))
         (next (world-next world))
         (neighbors (count-neighbors current i j)))
    ;; set the next bit pattern
    (if (zerop (bit current i j))
	(cond ((not (= neighbors 3))
	       ;; current = 0, surrounding cells != 3
	       (setf (bit next i j) 0))	
	      (t (setf (bit next i j) 1)
		 ;; current = 0, surrounding cells = 3
		 (incf (world-numdots world)))) 
        (cond ((or (= neighbors 2) 
		   (= neighbors 3))
	       ;; current = 1, surrounding cells = 2,3
	       (setf (bit next i j) 1))	
	      (t (setf (bit next i j) 0)
                 (decf (world-numdots world)))))
    ;; reset the bounds, if necessary
    (unless (zerop (bit next i j))
      (when (< i (world-xmin world)) (setf (world-xmin world) i))
      (when (> i (world-xmax world)) (setf (world-xmax world) i))
      (when (< j (world-ymin world)) (setf (world-ymin world) j))
      (when (> j (world-ymax world)) (setf (world-ymax world) j)))))

(defun count-neighbors (array i j)
  (+ (bit array (1- i) (1- j))
     (bit array i      (1- j))
     (bit array (1+ i) (1- j))
     (bit array (1- i) j)
     (bit array (1+ i) j)
     (bit array (1- i) (1+ j))
     (bit array i      (1+ j))
     (bit array (1+ i) (1+ j))))

(defun next-cycle (world)
  (let* ((lim (world-size world))
         (current (world-current world))
         (next (world-next world))
         (xlb (max 1 (1- (world-xmin world))))
         (xub (min (- lim 2) (1+ (world-xmax world))))
         (ylb (max 1 (1- (world-ymin world))))
         (yub (min (- lim 2) (1+ (world-ymax world)))))
    (dotimes (i (1+ (- xub xlb)))
      (dotimes (j (1+ (- yub ylb)))
        (setnext world (+ i xlb) (+ j ylb))))
    (dotimes (y lim)
      (dotimes (x lim)
        (setf (bit current x y) (bit next x y))))))

(defun print-world (world generations)
  (let ((lim (world-size world))
        (current (world-current world)))
    (dotimes (y lim)
      (dotimes (x lim)
        (if (zerop (bit current y x))
            (princ " ")
            (princ "*")))
      (terpri))
    (format t "~&~d Generations, ~d Organisms." 
            generations (world-numdots world))))

(defun propagate (world cycles)
  (print-world world cycles)
  (do ()
      ((zerop (world-numdots world))
       (format t "~2&POPULATION 0 ... ~d generations" cycles))
    (next-cycle world)
    (incf cycles)
    (print-world world cycles)))



(defun life (source)
  (let* ((size (length (car source)))
         (life (make-world
                :size size
                :current (make-array (list size size) :element-type 'bit
                                                      :initial-contents source)
                :next (make-array (list size size) :element-type 'bit
                                                   :initial-element 0)
                :numdots 0)))
    (dotimes (i size)
      (dotimes (j size)
        (unless (zerop (bit (world-current life) i j))
          (incf (world-numdots life))
          (when (< i (world-xmin life)) (setf (world-xmin life) i))
          (when (> i (world-xmax life)) (setf (world-xmax life) i))
          (when (< j (world-ymin life)) (setf (world-ymin life) j))
          (when (> j (world-ymax life)) (setf (world-ymax life) j)))))
    (propagate life 0)))

#|
;;; Example:
(setq test 
'((0 0 0 0 0 0 0 0)
(0 0 0 1 1 0 1 0)
(0 0 1 0 1 0 1 0)
(0 0 1 1 1 0 0 0)
(0 1 0 0 1 1 1 0)
(0 1 1 1 0 0 0 0)
(0 0 0 1 1 0 1 0)
(0 0 0 0 0 0 0 0)))

(life test)
|#

;;; *EOF*
