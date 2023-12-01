(defpackage :coral-ui
  (:use #:clim #:clim-lisp #:coral-tictactoe)
  (:export #:main))

(in-package :coral-ui)

(defparameter *box-size* 75)
(defparameter *box-sym-offset* 5)

(defparameter *board* (make-instance 'board))
(defparameter *player1* (make-instance 'player :board *board* :sym "X"))
(defparameter *player2* (make-instance 'player :board *board* :sym "O"))
(defparameter *winner* nil)

(defclass board-view (gadget-view) ())
(defclass field ()
  ((x :initarg :x :reader x)
   (y :initarg :y :reader y)
   (player-sym :initform nil :accessor player-sym)))
(defmethod initialize-instance :after ((field field) &key)
  (with-slots (player-sym x y) field
    (setf player-sym (aref (positions *board*) x y))))

(defparameter *fields* (make-array '(3 3)))
(dotimes (i 3)
  (dotimes (j 3)
    (setf (aref *fields* i j)
	  (make-instance 'field :x i :y j))))
(defun get-field (i j)
  (aref *fields* i j))
(defmethod (setf field) (val (field field))
  (with-slots (player-sym) field
    (setf player-sym val)
    (setf (aref (positions *board*) (x field) (y field)) val)))
(defun clear-fields ()
  (dotimes (i 3)
    (dotimes (j 3)
      (setf (field (get-field i j)) nil))))
(defun new-game ()
  (clear-fields)
  (setf *winner* nil))

(define-presentation-method present
    ((field field) (type field) stream (view board-view) &key)
  (with-slots (x y (sym player-sym)) field
    (let ((offset 0.07))
      (cond
	((string-equal sym "X")
	 (draw-line* stream
		     (+ x offset) (+ y offset)
		     (- (+ x 1) offset) (- (+ y 1) offset)
		     :ink +blue+ :line-thickness 5)
	 (draw-line* stream
		     (+ x offset) (- (+ y 1) offset)
		     (- (+ x 1) offset) (+ y offset)
		     :ink +blue+ :line-thickness 5))
	((string-equal sym "O")
	 (draw-circle* stream
		       (+ x .5) (+ y .5) (- .5 offset)
		       :filled nil :ink +red+
		       :line-thickness 5))
	(t (draw-rectangle* stream
			    (+ x offset) (+ y offset)
			    (- (+ x 1) offset) (- (+ y 1) offset)
			    :ink +white+))))))

(defun draw (frame stream)
    (with-scaling (stream *box-size* *box-size*)
      (if *winner*
	  (draw-text* stream
		      (concatenate 'string
				   (sym *winner*)
				   " Victory!")
		      1.5 3.25
		      :align-x :center)
	  (draw-text* stream
		      (concatenate 'string
				   "Player "
				   (sym (current-player frame)))
		      1.5 3.25
		      :align-x :center))
      (draw-line* stream 1 0 1 3)
    (draw-line* stream 0 1 3 1)
    (draw-line* stream 2 0 2 3)
    (draw-line* stream 0 2 3 2)
    (with-slots ((pos positions)) *board*
      (dotimes (i 3)
	(dotimes (j 3)
	  (present (get-field i j) 'field :stream stream))))))

(define-application-frame coral-clim ()
  ((current-player
    :initform *player1*
    :accessor current-player))
  (:panes
   (app :application
	:scroll-bars nil
	:width (* 3 *box-size*)
	:height (* 3.5 *box-size*)
	:max-width (* 3 *box-size*)
	:max-height (* 3.5 *box-size*)
	:display-function 'draw
	:default-view (make-instance 'board-view))))

(defun test-field (field &rest args)
  (declare (ignore args))
  (and
   (not (player-sym field))
   (not *winner*)))

(define-command (com-play-field :name t :command-table coral-clim)
    ((field 'field :gesture (:select :tester test-field)))
  (print "test")
  (let* ((frame *application-frame*)
	 (player (current-player frame)))
    (setf (field field) (sym player))
    (if (player-won player)
	(setf *winner* player)
	(cond
	  ((equal player *player1*)
	   (setf (current-player frame) *player2*))
	  ((equal player *player2*)
	   (setf (current-player frame) *player1*))
	  (t (setf (current-player frame) *player1*))))))

(defun main ()
  (new-game)
  (run-frame-top-level
   (make-application-frame 'coral-clim)))
