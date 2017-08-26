;;;; LOCATION A

;;;SCRIPT
;;; bottom-entry: fade-in, while moving from below


(defconstant +location-a-intro-length+ 5000.0 "How much time it takes to stabilize picture (ie. to stop parallaxing everything...)")
(defparameter *game-loc-a-sky-broken* nil "Have we seen the sky-broken animation yet?")
(defparameter *game-loc-a-sky-fixed* nil "Have we fixed the sky already?")
(defconstant +location-a-sky-break-time+ 15000.0 "When the sky goes to hell?")

(defparameter *location-a-timer* 0)


(defparameter *location-a* nil)

(defconstant +flag-shutdown-time+ 600)
(defconstant +fans-shutdown-time+ 600)

;;; LOCATION A OBJECTS
(defclass loc-a-transition (active-object)
  ((position :initform #(0 0))
   (size :initform #(42 42))
   (idle-animation :initform (get-animation "UI_down"))
   (active-position :initform #(0 0))
   (clickable :initform t)
   (active-size :initform #(42 42))))

(defmethod on-click ((lat loc-a-transition))
  (change-location-to :location-b))

(defclass loc-a-flag (object)
  ((position :initform #(0 0))
   (size :initform #(100 75))
   (shutdown-time-accu :initform 0)
   (state :initform :idle)
   (idle-animation :initform (get-animation "A_flaga_wiatr_pop1" 69))
   (nowind-animation :initform (get-animation "A_flaga_brak_witru_pop1" 75))))

(defmethod update ((flag loc-a-flag))
  (with-slots (idle-animation nowind-animation shutdown-time-accu state) flag
	(case state
	  (:idle (update-animation idle-animation))
	  (:shutting-down (progn (update-animation idle-animation)
							 (update-animation nowind-animation)
							 (incf shutdown-time-accu (sdl:dt))))
	  (:nowind (update-animation nowind-animation)))))

(defmethod draw ((flag loc-a-flag))
  (with-slots (position size idle-animation nowind-animation shutdown-time-accu state) flag
	(case state
	  (:idle (draw-animation idle-animation :dimmensions size :position position :color #(1.0 1.0 1.0 0.8)))
	  (:shutting-down (progn (draw-animation idle-animation
											 :dimmensions size
											 :position position
											 :color (vector 1.0 1.0 1.0 (lerp 1.0 0.0 (/ shutdown-time-accu +flag-shutdown-time+))))
							 (draw-animation nowind-animation
											 :dimmensions size
											 :position position
											 :color (vector 1.0 1.0 1.0 (lerp 0.0 1.0 (/ shutdown-time-accu +flag-shutdown-time+))))))
	  (:nowind (draw-animation nowind-animation :dimmensions size :position position :color #(1.0 1.0 1.0 0.8))))))

(defclass loc-a-fans (object)
  ((size :initform #(300 250))
   (state :initform :idle)
   (shutdown-time-accu :initform 0)
   (idle-animation :initform (get-animation "A_Wiatraki_z_pradem" 100))
   (shutdown-animation :initform (get-animation "A_Wiatrak" 100))))


;; COPY PASTA

(defmethod update ((fans loc-a-fans))
  (with-slots (idle-animation shutdown-animation shutdown-time-accu state) fans
	(case state
	  (:idle (update-animation idle-animation))
	  (:shutting-down (progn (update-animation idle-animation)
							 (update-animation shutdown-animation)
							 (incf shutdown-time-accu (sdl:dt))))
	  (:shut-down (update-animation shutdown-animation)))))

(defmethod draw ((fans loc-a-fans))
  (with-slots (position size idle-animation shutdown-animation shutdown-time-accu state) fans
	(case state
	  (:idle (draw-animation idle-animation :dimmensions size :position position))
	  (:shutting-down (progn (draw-animation idle-animation
											 :dimmensions size
											 :position position
											 :color (vector 1.0 1.0 1.0 (lerp 1.0 0.0 (/ shutdown-time-accu +fans-shutdown-time+))))
							 (draw-animation shutdown-animation
											 :dimmensions size
											 :position position
											 :color (vector 1.0 1.0 1.0 (lerp 0.0 1.0 (/ shutdown-time-accu +fans-shutdown-time+))))))
	  (:shut-down (draw-animation shutdown-animation :dimmensions size :position position)))))

(defclass loc-a-rupture (active-object)
  ((size :initform #(800 756))
   (active-position :initform #(0 0))
   (active-size :initform #(0 0))
   (idle-animation :initform (get-animation "A_AfterHit_Particle" 25 nil))
   (damage-animation :initform (get-animation "A_Hit_plus_particle" 100 nil))
   (damaged-animation :initform (get-animation "A_AfterHit_Particle" 50))
   (fix-animation :initform (get-animation "A_Clay_Heal_Animation" 100 nil))
   (fixed-animation :initform (get-animation "A_Clay_Healed" 100 nil))
   (damage-decal :initform (make-me-a-sprite "A_Crack.png"))
   (state :initform :prepping-to-break)
   (time-accumulator :initform 0)))


(defmethod update ((rupture loc-a-rupture))
  (with-slots (state time-accumulator damage-animation damaged-animation fix-animation) rupture
	(case state
	  (:fixing (progn
				 (if (animation-end-p fix-animation)
					 (progn
					   (setf state :fixed)
					   (setf *game-loc-a-sky-fixed* t)
					   ;;NOTE we assume here that the player won't be able to fix the firmanent in the first 30 seconds of his play...
					   (defer-event (deferred-to-n-msec-from-now 500 (lambda ()
																	   (stop-sound :wiatr-loop)
																	   (stop-sound :rupture)
																	   (stop-sound :sypanie))))
					   (defer-event (deferred-to-n-msec-from-now 1000 #'world-shutdown)))
					 (update-animation fix-animation))))
	  (:broken (update-animation damaged-animation))
	  (:breaking (progn
				   (if (animation-end-p damage-animation)
					   (progn
						 (setf state :broken)
						 (setf *game-loc-a-sky-broken* t))
					   (update-animation damage-animation))))
	  (:prepping-to-break (progn
							(if (> time-accumulator +location-a-sky-break-time+)
								(progn
								  (setf state :breaking)
								  (play-sound :rupture nil)
								  (defer-event (deferred-to-n-msec-from-now 14000 (lambda () (play-sound :sypanie t))))
								  ;;bells
								  (defer-event (deferred-to-n-msec-from-now 3000 (lambda () (start-dzwony))))
								  (defer-event (deferred-to-n-msec-from-now 2500 (lambda () (start-wind))))
								  (defer-event (deferred-to-n-msec-from-now 1500 (lambda () (format t "DEFERRED!"))))
								  (print time-accumulator))))))
	(incf time-accumulator (sdl:dt))))

(defmethod draw ((rupture loc-a-rupture))
  (with-slots (state fix-animation damage-animation damaged-animation fixed-animation position size damage-decal) rupture
	(case state
	  (:fixing (progn
				 (draw-me-a-sprite damage-decal :position position :dimmensions size)))
				 (draw-animation fix-animation :position position :dimmensions size)
	  (:broken (progn
				 (draw-animation damaged-animation :position position :dimmensions size)
				 (draw-me-a-sprite damage-decal :position position :dimmensions size)))
	  (:breaking (draw-animation damage-animation :position position :dimmensions size))
	  (:fixed (draw-animation fixed-animation :position position :dimmensions size)))))

(defmethod on-click ((rupture loc-a-rupture))
  (with-slots (state) rupture
	(if (item-selected-p :clay)
		(progn (remove-item :clay)
			   (setf state :fixing))
		;;DEBUG
		#+nil(setf state :fixing))))

(defclass loc-a-clouds (object)
  ((size :initform #(800 756))
   (idle-animation :initform (get-animation "A_Chmurki" 75))))

(defclass loc-a-dust (object)
  ((size :initform #(800 756))
   (idle-animation :initform (get-animation "A_Dust" 75))))

;;; LOCATION
(defclass location-a (location)
  ((background :initform (make-me-a-sprite "A_background.jpg"))
   (foreground :initform (make-me-a-sprite "A_katedra.png"))
   (state :initform :entering-from-below)
   (time-accumulator :initform 0)))

(defmethod initialize-location ((loc location-a))
  (with-slots (background objects) loc
	;(setf location 
	(setf (gethash :flag-left objects) (make-instance 'loc-a-flag :position #(333 692.5)))
	(setf (gethash :flag-right objects) (make-instance 'loc-a-flag :position #(683 702.5)))
	(setf (gethash :rupture objects) (make-instance 'loc-a-rupture :position #(0 40) :active-position #(92 441) :active-size #(42 28) :clickable t))
	(setf (gethash :clouds objects) (make-instance 'loc-a-clouds :position #(0 100)))
	(setf (gethash :dust objects) (make-instance 'loc-a-dust :position #(0 0)))
	(setf (gethash :goto-location-b objects) (make-instance 'loc-a-transition))
	(setf (gethash :fans objects) (make-instance 'loc-a-fans :position #(90 372)))
	
	;;SOUNDS
	(add-sound :rupture (load-me-some-sound "A_pekniecie.wav"))
	(add-sound :wiatr-loop (load-me-some-sound "A_wiatr_loop.wav"))
	(add-sound :dzwony-i-modlitwa (load-me-some-sound "A_dzwony_i_modlitwa.wav"))
	(add-sound :modlitwa-loop (load-me-some-sound "A_modlitwa_loop.wav"))
	(add-sound :sypanie (load-me-some-sound "A_sypanie_loop.wav"))
	
	(setf *location-a* loc))) ;;<-- singletonize

(defmethod deinitialize-location ((loc location-a))
  nil)

(defmethod update ((loc location-a))
  (incf *location-a-timer* (sdl:dt))
;;  (process-events)
  (with-slots (time-accumulator) loc
	(incf time-accumulator (sdl:dt))
	(call-next-method)))

(defmethod draw ((loc location-a))
  (with-slots (background foreground objects time-accumulator) loc

	;;NOTE HERE YOU CAN TWEAK ANIMATION STUFF
	(let ((foreground-y-translation (lerp 0 -200 (clamp (/ time-accumulator +location-a-intro-length+) 0 1)))
		  (background-y-translation (lerp -100 -156 (clamp (/ time-accumulator +location-a-intro-length+) 0 1))))
	  (gl:translate 0 background-y-translation 0)
	  (draw-me-a-sprite background :position #(0 0) :dimmensions #(800 756))
	  (gl:translate 0 (- background-y-translation) 0)

	  (gl:translate 0 foreground-y-translation 0)
	  ;; BACKGROUND OBJECTS
	  (mapc #'draw (mapcar (lambda (key) (gethash key objects)) '(:rupture :clouds :fans)))
	  (draw-me-a-sprite foreground :position #(0 0) :dimmensions #(800 756))
	  ;; FOREGROUND OBJECTS
	  (mapc #'draw (mapcar (lambda (key) (gethash key objects)) '(:flag-left :flag-right :dust :goto-location-b)))
	  (gl:translate 0 (- foreground-y-translation) 0))))

(defmethod enter-location ((loc location-a) direction)
  (declare (ignore direction))
  (with-slots (state objects) loc

	(if *game-loc-a-sky-broken*
		(progn (setf (slot-value (gethash :rupture objects) 'state) :broken)
			   (play-sound :modlitwa-loop t)
			   (play-sound :wiatr-loop t)))

	(if *game-loc-a-sky-fixed*
		(progn
		  (setf (slot-value (gethash :rupture objects) 'state) :fixed)
		  (world-shutdown)))

	(setf state :entering-from-below)
	(set-to-random-frame (slot-value (gethash :flag-left objects) 'idle-animation))
	(set-to-random-frame (slot-value (gethash :flag-right objects) 'idle-animation))))

(defun start-dzwony ()
  (play-sound :dzwony-i-modlitwa nil)
  (defer-event (deferred-to-n-msec-from-now 35000 (lambda ()
													(format t "playing modlitwa loop")
													(play-sound :modlitwa-loop t)))))

(defun start-wind ()
  (format t "Firing wind sounds...~%")
  (play-sound :wiatr-loop t))

(defun world-shutdown ()
  (with-slots (objects) *location-a*

	(defer-event (deferred-to-n-msec-from-now 1000
					 (lambda ()
					   (shutdown-object (gethash :flag-left objects)))))

	(defer-event (deferred-to-n-msec-from-now 2000
					 (lambda ()
					   (shutdown-object (gethash :flag-right objects)))))

	(defer-event (deferred-to-n-msec-from-now 1
					 (lambda ()
					   (shutdown-object (gethash :fans objects)))))))

(defmethod shutdown-object ((flag loc-a-flag))
  (with-slots (state) flag
	(setf state :shutting-down)
	(defer-event (deferred-to-n-msec-from-now +flag-shutdown-time+ (lambda ()
																	 (setf state :nowind))))))

(defmethod shutdown-object ((fans loc-a-fans))
  (with-slots (state) fans
	(setf state :shutting-down)
	(defer-event (deferred-to-n-msec-from-now +fans-shutdown-time+ (lambda ()
																	 (setf state :shut-down))))))