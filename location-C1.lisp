;;; LOCATION C w/ following conditions:
;;; + we have electricity
;;; + we have a mechanician

(defclass loc-c1-transition (active-object)
  ((position :initform #(0 0))
   (size :initform #(42 42))
   (idle-animation :initform (get-animation "UI_up"))
   (active-position :initform #(0 0))
   (clickable :initform t)
   (active-size :initform #(42 42))))

(defmethod on-click ((lat loc-c1-transition))
  (change-location-to :location-b))

(defclass loc-c1-texturedlight (object)
  ((position :initform #(0 0))
   (size :initform #(800 600))
   (idle-animation :initform (get-animation "C_textured_light_early" 80))))

(defclass loc-c1-kobyla (object)
  ((position :initform #(0 0))
   (size :initform #(800 600))
   (idle-animation :initform (get-animation "C_Kobyla" 80))))

(defclass location-c1 (location)
  ((background :initform (make-me-a-sprite "C_background.jpg"))
   (mechanist :initform (make-me-a-sprite "C_mechanist.png"))
   (silownik :initform (make-me-a-sprite "C_silownik.png"))))

(defmethod initialize-location ((loc location-c1))
  (with-slots (objects) loc
	(setf (gethash :kobyla objects) (make-instance 'loc-c1-kobyla))
	(setf (gethash :gotob objects) (make-instance 'loc-c1-transition))
	(setf (gethash :texturedlight objects) (make-instance 'loc-c1-texturedlight))
	
	;;some sounds
	(add-sound :c1-ambient (load-me-some-sound "C_sekwencja_w_tle.wav"))
	(add-sound :c1-electrical-ambient (load-me-some-sound "C_tlo.wav"))

	(play-sound :c1-ambient t)
	(play-sound :c1-electrical-ambient t)))

(defmethod deinitialize-location ((loc location-c1))
  nil)

(defmethod enter-location ((loc location-c1) direction)
  nil)

(defmethod update ((loc location-c1))
  (call-next-method))

(defmethod draw ((loc location-c1))

  (with-slots (background mechanist silownik objects) loc

  ;;Draw background
	(draw-me-a-sprite background :position #(0 0) :dimmensions #(800 600))
	(draw-me-a-sprite silownik :position #(0 0) :dimmensions #(800 600))
	(draw-me-a-sprite mechanist :position #(0 0) :dimmensions #(800 600))

	(mapc #'draw (mapcar (lambda (key) (gethash key objects)) '(:kobyla :texturedlight)))))

