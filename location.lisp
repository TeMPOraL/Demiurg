;;; LOCATION concepts

(defclass location ()
  ((objects :initform (make-hash-table :test #'eql))
   (active-objects :initform (make-hash-table :test #'eql))))

(defgeneric initialize-location (location))

(defgeneric deinitialize-location (location))

(defmethod draw ((loc location))
  nil)

(defmethod update ((loc location))
  (maphash (lambda (k v)
			 (declare (ignore k))
			 (update v))
		   (slot-value loc 'objects)))


(defclass object ()
  ((position
	:initarg :position
	:initform (error "Position unspecified"))
   (size
	:initarg :size
	:initform (error "Size unspecified"))
   (idle-animation
	:initarg :idle-animation
	:initform (error "Idle animation unspecified"))))

(defmethod update ((obj object))
  (process-events)
  (with-slots (idle-animation) obj
	(update-animation idle-animation)))

(defmethod draw ((obj object))
  (with-slots (position size idle-animation) obj
	(draw-animation idle-animation :position position :dimmensions size)))


(defclass active-object (object)
  ((active-position
	:initarg :active-position
	:initform (error "Active position unspecified"))
   (active-size
	:initarg :active-size
	:initform (error "Active position unspecified"))
   (clickable
	:initarg :clickable
	:initform nil)))

(defmethod mouse-over-object-p ((object active-object))
  (with-slots (active-position active-size clickable) object
	(if clickable
		(mouse-in-a-box-p (aref active-position 0)
						  (aref active-position 1)
						  (aref active-size 0)
						  (aref active-size 1))
		nil)))

(defmethod mouse-over-object-p (object)
  (declare (ignore object))
  nil)

(defmethod handle-mouse-click ((loc location))
  (with-slots (objects) loc
	  (maphash (lambda (k v)
				 (declare (ignore k))
				 (if (mouse-over-object-p v)
					 (on-click v)))
			   objects)))