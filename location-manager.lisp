;;;; Location Manager
;;; Something like my typical Game State Manager, but different ;)
(defparameter *current-location* nil)
(defparameter *target-location* nil)

(defparameter *current-world-state* :fade-in)

(defparameter *current-fade-timer* 0)

(defconstant +location-fade-time+ 6000)

;; 4 different states: :fade-in :fade-out :reallocating :normal
;; :reallocating might not be counted as state


;;; PUBLIC INTERFACE ;)
(defun update-game-world ()
  (case *current-world-state*
	(:normal (progn
			   (update-event-timer)
			   (update *current-location*)))
	(:fade-in (progn ;;FIXME fade-in volume
				(update-event-timer)
				(update *current-location*)
				(incf *current-fade-timer* (sdl:dt))
				(when (> *current-fade-timer* +location-fade-time+)
				  (setf *current-world-state* :normal))))
	(:fade-out (progn ;;FIXME fade-out volume
				 (update-event-timer)
				 (update *current-location*)
				 (incf *current-fade-timer* (sdl:dt))
				 (when (> *current-fade-timer* +location-fade-time+)
				   (dismantle-location *current-location*)
				   (setf *current-location* (create-new-location *target-location*))
				   (initialize-this-newly-created-location *current-location*)
				   (setf *current-world-state* :fade-in)
				   (setf *current-fade-timer* 0))))))

(defun draw-game-world ()
  (case *current-world-state*
	(:normal (draw *current-location*))
	(:fade-in (progn
				(draw *current-location*)
				(draw-big-black-square (lerp 1.0 0.0 (/ *current-fade-timer* +location-fade-time+)))))
	(:fade-out (progn
				 (draw *current-location*)
				 (draw-big-black-square (lerp 0.0 1.0 (/ *current-fade-timer* +location-fade-time+)))))))


(defun change-location-to (new-location-id)
  (setf *current-fade-timer* 0)
  (setf *target-location* new-location-id)
  (setf *current-world-state* :fade-out))

(defun setup-with-location (location)
  (setf *current-location* (create-new-location location))
  (initialize-this-newly-created-location *current-location*)
  (setf *current-world-state* :fade-in)
  (setf *current-fade-timer* 0))

;;; PRIVATE IMPLEMENTATION

(defun handle-input-dispatching ()
  ;;; block controls during fade-in / fade-out
  (case *current-world-state*
	(:normal (dispatch-input *current-location*))
	(otherwise nil)))

(defun dismantle-location (location)
  (deinitialize-location location)
  (drop-all-animations)
  (stop-all-sounds)
  (clear-event-data))

(defun create-new-location (location)
  (case location
	(:intro (make-instance 'location-intro))
	(:location-a (make-instance 'location-a))
	(:location-b (make-instance 'location-b))
	(:location-c (if (and *game-loc-a-sky-broken* (not *game-loc-a-sky-fixed*))
					 (make-instance 'location-c1)
					 (if *game-loc-a-sky-fixed*
						 (make-instance 'location-c2))))
	(:location-c3 (make-instance 'location-c3))
	;;TODO more
	(otherwise (error "Invalid location specified!"))))

(defun initialize-this-newly-created-location (location)
  (initialize-location location)
  (enter-location location :somewhere))


(defun draw-big-black-square (alpha)
  (gl:color 0.0 0.0 0.0 alpha) ;;<-- NOTE might screw up colors on the location!
  (gl:disable :texture-2d)
  (gl:with-primitive :quads
	(gl:vertex 0 0 0)
	(gl:vertex 800 0 0)
	(gl:vertex 800 600 0)
	(gl:vertex 0 600 0)
  
	(gl:vertex 0 0 0)
	(gl:vertex 0 600 0)
	(gl:vertex 800 600 0)
	(gl:vertex 800 0 0))
  (gl:enable :texture-2d))