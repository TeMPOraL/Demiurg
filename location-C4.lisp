;;; LOCATION C w/ following conditions:
;;; + we have electricity
;;; + we don't have a mechanician
;;; + there's blood everywhere!

(defclass loc-c4-texturedlight (object)
  ((position :initform #(0 0))
   (size :initform #(800 600))
   (idle-animation :initform (get-animation "C_textured_light_no_mechanic" 80))))

(defclass loc-c4-kobyla (object)
  ((position :initform #(0 0))
   (size :initform #(800 600))
   (idle-animation :initform (get-animation "C_Kobyla" 80))))

(defclass location-c4 (location)
  ((background :initform (make-me-a-sprite "C_background_after_slaughter.jpg"))))

(defmethod initialize-location ((loc location-c4))
  (with-slots (objects) loc
	(setf (gethash :kobyla objects) (make-instance 'loc-c4-kobyla))
	(setf (gethash :texturedlight objects) (make-instance 'loc-c4-texturedlight))

	;;some sounds
	(add-sound :c4-ambient (load-me-some-sound "C_sekwencja_w_tle.wav"))
	(add-sound :c4-electrical-ambient (load-me-some-sound "C_tlo.wav"))

	(play-sound :c4-ambient t)
	(play-sound :c4-electrical-ambient t)))


(defmethod deinitialize-location ((loc location-c4))
  nil)

(defmethod enter-location ((loc location-c4) direction)
  nil)

(defmethod update ((loc location-c4))
  (call-next-method))

(defmethod draw ((loc location-c4))

  (with-slots (background  objects) loc

  ;;Draw background
	(draw-me-a-sprite background :position #(0 0) :dimmensions #(800 600))

	(mapc #'draw (mapcar (lambda (key) (gethash key objects)) '(:kobyla :texturedlight)))))

