;;; LOCATION C w/ following conditions:
;;; + we don't have electricity
;;; + we don't have a mechanician
;;; + there's blood everywhere

(defclass loc-c3-transition (active-object)
  ((position :initform #(0 0))
   (size :initform #(42 42))
   (idle-animation :initform (get-animation "UI_up"))
   (active-position :initform #(0 0))
   (clickable :initform t)
   (active-size :initform #(42 42))))

(defmethod on-click ((lat loc-c3-transition))
  (change-location-to :location-b))

(defclass loc-c3-texturedlight (object)
  ((position :initform #(0 0))
   (size :initform #(800 600))
   (idle-animation :initform (get-animation "C_textured_light_after_slaughter" 80))))

(defclass loc-c3-kobyla (object)
  ((position :initform #(0 0))
   (size :initform #(800 600))
   (idle-animation :initform (get-animation "C_Kobyla" 80))))

(defclass location-c3 (location)
  ((background :initform (make-me-a-sprite "C_background_after_slaughter.jpg"))
   (heart :initform (make-me-a-sprite "C_heart.png"))))

(defmethod initialize-location ((loc location-c3))
  (with-slots (objects) loc
	(setf (gethash :kobyla objects) (make-instance 'loc-c3-kobyla))
	(setf (gethash :gotob objects) (make-instance 'loc-c3-transition))
	(setf (gethash :texturedlight objects) (make-instance 'loc-c3-texturedlight))

	;;some sounds
	(add-sound :c3-ambient (load-me-some-sound "C_sekwencja_w_tle.wav"))
	(add-sound :c3-candle-ambient (load-me-some-sound "C_swieczka.wav"))

	(play-sound :c3-ambient t)
	(play-sound :c3-candle-ambient t)))

(defmethod deinitialize-location ((loc location-c3))
  nil)

(defmethod enter-location ((loc location-c3) direction)
  nil)

(defmethod update ((loc location-c3))
  (call-next-method))

(defmethod draw ((loc location-c3))

  (with-slots (background heart objects) loc

  ;;Draw background
	(draw-me-a-sprite background :position #(0 0) :dimmensions #(800 600))
	(draw-me-a-sprite heart :position #(0 0) :dimmensions #(800 600))

	(mapc #'draw (mapcar (lambda (key) (gethash key objects)) '(:kobyla :texturedlight)))))

