(defpackage :coral-tictactoe
  (:use #:cl)
  (:export board positions sym player player-won))

(in-package :coral-tictactoe)

(defclass board ()
  ((positions
    :initform (make-array '(3 3) :initial-element nil)
    :accessor positions)))

(defclass player ()
  ((board
    :initarg :board
    :accessor board)
   (sym
     :initarg :sym
     :accessor sym)))

(defmethod player-won-row ((p player))
  (with-slots ((b board) sym) p
    (with-slots ((pos positions)) b
      (let ((count 0))
	(do ((i 0 (1+ i)))
	    ((or (= i 3) (= count 3)) (= count 3))
	  (setf count 0)
	  (do ((j 0 (1+ j)))
	      ((= j 3) (= count 3))
	    (when (string-equal (aref pos i j) sym)
	      (setf count (1+ count)))))))))

(defmethod player-won-column ((p player))
  (with-slots ((b board) sym) p
    (with-slots ((pos positions)) b
      (let ((count 0))
	(do ((i 0 (1+ i)))
	    ((or (= i 3) (= count 3)) (= count 3))
	  (setf count 0)
	  (do ((j 0 (1+ j)))
	      ((= j 3) (= count 3))
	    (when (string-equal (aref pos j i) sym)
	      (setf count (1+ count)))))))))

(defmethod player-won-diagonal ((p player))
  (with-slots ((b board) sym) p
    (with-slots ((pos positions)) b
      (let ((count 0))
	(or
	 (do ((i 0 (1+ i)))
	     ((= i 3) (= count 3))
	   (when (string-equal (aref pos i i) sym)
	     (setf count (1+ count))))
	 (do ((i 0 (1+ i))
	      (count 0 count))
	     ((= i 3) (= count 3))
	   (when (string-equal (aref pos i (- 2 i)) sym)
	     (setf count (1+ count)))))))))

(defmethod player-won ((p player))
  (with-slots ((b board) sym) p
    (with-slots ((pos positions)) b
      (or
       (player-won-row p)
       (player-won-column p)
       (player-won-diagonal p)))))
