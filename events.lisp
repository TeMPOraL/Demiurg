;;;; DEFERRED EVENT QUEUE
(defparameter *deferred-event-queue* '())
(defparameter *events-just-deferred* '())
(defparameter *global-event-timer* 0)

(defun defer-event (event)
  (push event *events-just-deferred*))

(defun process-events ()
  (setf *deferred-event-queue* (append (mapcan #'funcall *deferred-event-queue*) *events-just-deferred*))
  (setf *events-just-deferred* nil))

(defun n-msec-from-now (msec)
  (let ((target-time (+ *global-event-timer* msec)))
	(lambda ()
	  (> *global-event-timer* target-time))))

(defun deferred-until-condition-is-met (condition what)
  (labels ((make-it-so ()
			 (if (funcall condition)
				 (progn (funcall what)
						nil)
				 (list #'make-it-so))))
	#'make-it-so))

(defun deferred-to-n-msec-from-now (msec what)
  (deferred-until-condition-is-met (n-msec-from-now msec)
	  what))

(defun update-event-timer ()
  (incf *global-event-timer* (sdl:dt)))
		  

(defun clear-event-data ()
  (setf *deferred-event-queue* nil)
  (setf *events-just-deferred* nil)
  (setf *global-event-timer* 0))