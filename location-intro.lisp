(defclass loc-intro-bg (object)
  ((position :initform #(0 0))
   (size :initform #(800 600))
   (idle-animation :initform (get-animation "00_start_plansza" 80 t))))

(defclass loc-intro-txt (object)
  ((position :initform #(0 0))
   (size :initform #(800 600))
   (idle-animation :initform (get-animation "00_start_historia" 80 nil))))

(defclass location-intro (location)
  ())

(defmethod initialize-location ((loc location-intro))
  (with-slots (objects) loc
	(setf (gethash :bg objects) (make-instance 'loc-intro-bg))
	(setf (gethash :txt objects) (make-instance 'loc-intro-txt))

	(let ((txt (gethash :txt objects)))
	  (defer-event (deferred-until-condition-is-met
					   (lambda ()
						 (animation-end-p (slot-value txt 'idle-animation)))
					   (lambda ()
						 (change-location-to :location-a)))))))

(defmethod deinitialize-location ((loc location-intro))
  nil)

(defmethod enter-location ((loc location-intro) direction)
  nil)

(defmethod update ((loc location-intro))
  (call-next-method))

(defmethod draw ((loc location-intro))

  (with-slots (objects) loc

  ;;Draw background
	(mapc #'draw (mapcar (lambda (key) (gethash key objects)) '(:bg :txt)))))

