;;; INPUT UTILITIES

;;; INPUT
(defun mouse-position ()
  (let ((vec (sdl:mouse-position)))
	(vector (aref vec 0) (- +screen-height+ (aref vec 1)))))

(defun mouse-pos-3d ()
  "Return a 3D vector with mouse position."
  (let ((mouse-pos (sdl:mouse-position)))
	(vector (aref mouse-pos 0)
			(aref mouse-pos 1)
			0)))

(defun mouse-in-a-box-p (box-x box-y box-w box-h)
  (point-in-a-box-p (mouse-position) box-x box-y box-w box-h))