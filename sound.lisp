;;;; Toolkit with sound utilities

(defparameter *sounds* (make-hash-table :test #'eql))

(defun load-me-some-sound (name)
  (let* ((pathname (namestring (full-pathname (merge-pathnames name
															   +sfx-asset-path+))))
		 (src (al:gen-source))
		 (buffer (alut:create-buffer-from-file pathname)))
	(al:source src :buffer buffer)
	(list src buffer)))

(defun play-me-a-sound (sound &key positional position (looping nil)) ;;;FIXME change interface to something that makes sense
  (declare (ignore positional position))
  (al:source (car sound) :looping looping)
  (al:source-play (car sound)))


(defun play-sound (sound looping)
  (aif (gethash sound *sounds*)
	   (play-me-a-sound it :looping looping)))

(defun add-sound (sound sound-data)
  (setf (gethash sound *sounds*) sound-data))

(defun sound-playing-p (sound)
  (eql (al:get-source sound :source-state) :playing))

(defun stop-sound (sound)
  (aif (gethash sound *sounds*)
	   (al:source-stop (car it))))

(defun stop-all-sounds ()
  (maphash (lambda (k v)
			 (format t "Stopping sound: ~a~%" k)
			 (al:source-stop (car v)))
		   *sounds*))