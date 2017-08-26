;;; LOCATION C w/ following conditions:
;;; + we don't have electricity
;;; + we have a mechanician

(defclass loc-c2-transition (active-object)
  ((position :initform #(0 0))
   (size :initform #(42 42))
   (idle-animation :initform (get-animation "UI_up"))
   (active-position :initform #(0 0))
   (clickable :initform t)
   (active-size :initform #(42 42))))

(defmethod on-click ((lat loc-c2-transition))
  (change-location-to :location-b))

(defclass loc-c2-texturedlight (object)
  ((position :initform #(0 0))
   (size :initform #(800 600))
   (idle-animation :initform (get-animation "C_textured_light_no_electricity" 80))))

(defclass loc-c2-kobyla (object)
  ((position :initform #(0 0))
   (size :initform #(800 600))
   (idle-animation :initform (get-animation "C_Kobyla" 80))))

(defclass loc-c2-eyes (object)
  ((position :initform #(650 450))
   (size :initform #(50 50))
   (idle-animation :initform (get-animation "C_Eyes" 100))))

(defclass location-c2 (location)
  ((background :initform (make-me-a-sprite "C_background.jpg"))))

(defmethod initialize-location ((loc location-c2))
  (with-slots (objects) loc
	(setf (gethash :kobyla objects) (make-instance 'loc-c2-kobyla))
	(setf (gethash :gotob objects) (make-instance 'loc-c2-transition))
	(setf (gethash :eyes objects) (make-instance 'loc-c2-eyes))
	(setf (gethash :texturedlight objects) (make-instance 'loc-c2-texturedlight))

	;;some sounds
	(add-sound :c2-ambient (load-me-some-sound "C_sekwencja_w_tle.wav"))
	(add-sound :c2-candle-ambient (load-me-some-sound "C_swieczka.wav"))

	(add-sound :c2-slaughter (load-me-some-sound "C_zarzynanie.wav"))

	;;DEBUG
	(defer-event (deferred-to-n-msec-from-now 16000 #'do-slaughter))

	(play-sound :c2-ambient t)
	(play-sound :c2-candle-ambient t)))

(defmethod deinitialize-location ((loc location-c2))
  nil)

(defmethod enter-location ((loc location-c2) direction)
  nil)

(defmethod update ((loc location-c2))
  (call-next-method))

(defmethod draw ((loc location-c2))

  (with-slots (background mechanist silownik objects) loc

  ;;Draw background
	(draw-me-a-sprite background :position #(0 0) :dimmensions #(800 600))

	(mapc #'draw (mapcar (lambda (key) (gethash key objects)) '(:kobyla :texturedlight :eyes)))))

(defun do-slaughter ()
  (play-sound :c2-slaughter nil)
  (defer-event (deferred-to-n-msec-from-now 1000
				   (lambda ()
					 (change-location-to :location-c3)))))
