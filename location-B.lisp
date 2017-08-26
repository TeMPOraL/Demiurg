;;; LOCATION B - sth...

(defconstant +base-time-between-leaves-effect+ 10000)
(defconstant +additional-random-time-between-leaves-effect+ 15000)

(defconstant +robot-travel-duration+ 5000)
(defconstant +robot-mining-duration+ 5000)
(defconstant +robot-travelback-duration+ 5000)

(defparameter *location-b-please-hide-door* nil)

;;; NOTE watch out, the following two variables are also used to keep state between visits to this location!
(defparameter *clay-taken* nil)
(defparameter *clay-prepped* nil)

(defparameter *location-b* nil)

(defclass loc-b-transition (active-object)
  ((position :initform #(0 0))
   (size :initform #(42 42))
   (idle-animation :initform (get-animation "empty"))
   (active-position :initform #(0 0))
   (clickable :initform t)
   (active-size :initform #(42 42))))

(defmethod on-click ((lat loc-b-transition))
  (change-location-to :location-a))

(defclass loc-b-transition2 (active-object)
  ((position :initform #(64 0))
   (size :initform #(42 42))
   (idle-animation :initform (get-animation "UI_down"))
   (active-position :initform #(64 0))
   (clickable :initform t)
   (active-size :initform #(42 42))))

(defmethod on-click ((lat loc-b-transition2))
  (change-location-to :location-c))

(defclass loc-b-leaves (object)
  ((position :initform #(0 0))
   (size :initform #(800 600))
   (idle-animation :initform (get-animation "B_Leaves" 100 nil))))

(defclass loc-b-filar-lights (object)
  ((position :initform #(0 0))
   (size :initform #(250 250))
   (idle-animation :initform (get-animation "B_FilarLights" 50))))

(defclass loc-b-fog (object)
  ((position :initform #(0 0))
   (size :initform #(800 200))
   (idle-animation :initform (get-animation "B_Fog" 100))))

(defclass loc-b-robot (object)
  ((position :initform #(0 0))
   (size :initform #(800 600))
   (idle-animation :initform (get-animation "empty")) ;;fixme
   (comeforth-animation :initform (get-animation "B_RobotComingForth" 70 nil))
   (comeback-animation :initform (get-animation "B_RobotComingBack" 70 nil))
   (clay-holding-animation :initform (get-animation "B_RobotHoldingClay"))
   (noclay-animation :initform (get-animation "B_RobotNoClay"))))

(defclass loc-b-door-trigger (active-object)
  ((position :initform #(0 0))
   (size :initform #(64 64))
   (active-position :initform #(600 70))
   (active-size :initform #(190 400))
   (clickable :initform t)
   (idle-animation :initform (get-animation "empty"))))

(defmethod on-click ((trigger loc-b-door-trigger))
  (with-slots (clickable) trigger
	(setf clickable nil)
	(defer-event (deferred-to-n-msec-from-now 500
					 (lambda ()
					   (robot-scene-start (gethash :robot (slot-value *location-b* 'objects))))))))

(defclass loc-b-clay-grabber (active-object)
  ((position :initform #(0 0))
   (size :initform #(64 64))
   (active-position :initform #(600 70))
   (active-size :initform #(190 400))
   (clickable :initform nil)
   (idle-animation :initform (get-animation "empty"))))

(defmethod on-click ((trigger loc-b-clay-grabber))
  (with-slots (clickable) trigger
	(format t "Grabbed clay~%")
	(setf clickable nil)
	(give-item :clay)
	(play-sound :b-clay nil)
	(setf *clay-taken* t)))
	
(defclass location-b (location)
  ((background :initform (make-me-a-sprite "B_Background.jpg"))
   (door :initform (make-me-a-sprite "B_Door.png"))
   (sign :initform (make-me-a-sprite "B_Sign.png"))))

(defmethod initialize-location ((loc location-b))
  (with-slots (background objects) loc
	(setf *location-b* loc)
	(setf (gethash :filar-lights objects) (make-instance 'loc-b-filar-lights :position #(400 300)))
	(setf (gethash :fog objects) (make-instance 'loc-b-fog))
	(setf (gethash :robot objects) (make-instance 'loc-b-robot))
	(setf (gethash :door-trigger objects) (make-instance 'loc-b-door-trigger))
	(setf (gethash :clay-grabber objects) (make-instance 'loc-b-clay-grabber))
	(setf (gethash :goto-location-b objects) (make-instance 'loc-b-transition))
	(setf (gethash :goto-location-c objects) (make-instance 'loc-b-transition2))
	(setf (gethash :leaves objects) (make-instance 'loc-b-leaves))


	;; LOAD SOME SOUNDS PLZ
	(add-sound :b-prayer-loop (load-me-some-sound "B_modlitwa_loop.wav"))
	(add-sound :b-wind-loop (load-me-some-sound "B_wiatr_loop.wav"))
	(add-sound :b-robot (load-me-some-sound "B_sekwencja.wav"))
	(add-sound :b-clay (load-me-some-sound "B_clay.wav"))

	;; DO SOME LEVEL STUFF

	;; play wind loop, unless player fixes the sky
	(unless *game-loc-a-sky-fixed*
	  (play-sound :b-wind-loop t))

	;; prayers shall continue until the robot is seen
	(unless *clay-prepped*
	  (play-sound :b-prayer-loop t))

	(setf *location-b-please-hide-door* nil)
	(defer-event (deferred-until-condition-is-met (lambda ()
													*clay-prepped*)
					 (lambda ()
					   (format t "Disabling robot doors.~%")
					   (setf (slot-value (gethash :door-trigger objects) 'clickable) nil))))

	(defer-event (deferred-until-condition-is-met (lambda ()
													(and *clay-prepped* (not *clay-taken*)))
					 (lambda ()
					   (format t "Clay grabber marked as clickable.~%")
					   (setf (slot-value (gethash :clay-grabber objects) 'clickable) t))))

	;;leaves effect
	(defer-event (deferred-to-n-msec-from-now +base-time-between-leaves-effect+
					 (lambda ()
					   (leaves-flyby (gethash :leaves objects)))))))

(defmethod deinitialize-location ((loc location-b))
  nil)

(defmethod enter-location ((loc location-b) direction)
  (declare (ignore direction))
  ;;ensure that when we enter this location, we'll see the proper state
  (if (and *clay-prepped* *clay-taken*)
	  (progn
		(with-slots (objects) loc
		  (let ((robot (gethash :robot objects)))
			(with-slots (idle-animation noclay-animation) robot
			  (setf idle-animation noclay-animation))))))

  (if (and *clay-prepped* (not *clay-taken*))
	  (progn
		(with-slots (objects) loc
		  (let ((robot (gethash :robot objects)))
			(with-slots (idle-animation clay-holding-animation) robot
			  (setf idle-animation clay-holding-animation)
			  (defer-event (deferred-until-condition-is-met
							   (lambda ()
								 *clay-taken*)
							   (lambda ()
								 (robot-scene-finalize robot))))))))))

(defmethod draw ((loc location-b))
  (with-slots (background door sign objects) loc

	;;general background
	(draw-me-a-sprite background
					  :position #(0 0)
					  :dimmensions #(800 600))

	;;doors - only if not animating a robot
	(if (not *location-b-please-hide-door*)
		(draw-me-a-sprite door
						  :position #(0 0)
						  :dimmensions #(800 600)))

	;sign - usually
	(draw-me-a-sprite sign
					  :position #(0 0)
					  :dimmensions #(800 600))

	;;DRAW foreground stuff
	(mapc #'draw (mapcar (lambda (key) (gethash key objects)) '(:leaves :filar-lights :robot :fog)))))

					  
;;  (with-slots (objects) loc
;;	(draw (gethash :leafs objects))))

(defun leaves-flyby (leaves)
  ;;FIXME drop deferring leaves if electricity is OFF.
  (with-slots (idle-animation) leaves
	(reset-animation idle-animation))
  (format t "Leaves effect reset; deffering...~%")
  (defer-event (deferred-to-n-msec-from-now (+ +base-time-between-leaves-effect+ (random +additional-random-time-between-leaves-effect+))
				   (lambda ()
					 (leaves-flyby leaves)))))


;;; ROBOT SCENE
;;; A group of chained events!

(defun robot-scene-start (robot)
  (with-slots (idle-animation comeforth-animation) robot
	(setf idle-animation comeforth-animation)
	(reset-animation idle-animation)

	;;disable praying sound
	(play-sound :b-robot nil)

	(defer-event (deferred-to-n-msec-from-now 765
					 (lambda ()
					   	(stop-sound :b-prayer-loop))))
  
	(setf *location-b-please-hide-door* t)
	(defer-event (deferred-until-condition-is-met
					 (lambda ()
					   (animation-end-p idle-animation))
					 (lambda ()
					   (robot-scene-mine robot))))))

(defun robot-scene-mine (robot)
  (setf *location-b-please-hide-door* nil)
  (defer-event (deferred-to-n-msec-from-now +robot-mining-duration+
				   (lambda ()
					 (robot-scene-end robot)))))

(defun robot-scene-end (robot)
  (with-slots (idle-animation comeback-animation clay-holding-animation) robot
	(setf idle-animation comeback-animation)
	(reset-animation idle-animation)
	(defer-event (deferred-until-condition-is-met
					 (lambda ()
					   (animation-end-p idle-animation))
					 (lambda ()
					   (setf idle-animation clay-holding-animation)
					   (setf *clay-prepped* t))))
	(defer-event (deferred-until-condition-is-met
					 (lambda ()
					   *clay-taken*)
					 (lambda ()
					   (robot-scene-finalize robot))))))

(defun robot-scene-finalize (robot)
  (with-slots (idle-animation noclay-animation) robot
	(setf idle-animation noclay-animation)
	(reset-animation idle-animation)))