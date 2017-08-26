;;;; Image loading / drawing utilities.
(defun load-a-pic (image-name &key (color-key-at (make-vector-2d 0 0) color-key-supplied-p))
  (let ((pathname (namestring (full-pathname (merge-pathnames image-name
															  +gfx-asset-path+)))))
  (if color-key-supplied-p
      (sdl:load-image pathname :color-key-at color-key-at :alpha 255)
      (sdl:load-image pathname))))


(defun draw-a-pic (image &key
                   (position (make-vector-2d))
                   (dimmensions (make-vector-2d)))
  (sdl:draw-surface-at-* image
                          (round (- (elt position 0)
                                    (/ (elt dimmensions 0) 2)))
                          (round (- (elt position 1)
                                    (/ (elt dimmensions 1) 2)))))



;;; Found in some random web place; I don't remember where exactly - probably on some OpenGL tutorial site
;;; TODO FIXME find and link source of this code here.
(defun sdl-surface-to-opengl-texture (image)
  (let ((texture (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (sdl-base::with-pixel (pix (sdl:fp image))
			  ;; we should probably be a bit more intelligent about this, but this
			  ;; handles some common cases
			  (let ((texture-format (ecase (sdl-base::pixel-bpp pix)
						  (3 :rgb)
						  (4 :rgba))))
			    ;; we should also handle this properly, by adjusting the
			    ;; settings of gl:pixel-store
			    (assert (and (= (sdl-base::pixel-pitch pix)
					    (* (sdl:width image) (sdl-base::pixel-bpp pix)))
					 (zerop (rem (sdl-base::pixel-pitch pix) 4))))
			    (gl:tex-image-2d :texture-2d 0 :rgba
					     (sdl:width image) (sdl:height image)
					     0
					     texture-format
					     :unsigned-byte (sdl-base::pixel-data pix))))
    texture)) ; return opengl texture


(defun make-me-a-sprite (name)
  (let* ((picture (load-a-pic name))
		 (texture (sdl-surface-to-opengl-texture picture)))
	(sdl:free picture)
	texture))

(defun draw-me-a-sprite (sprite &key (position (make-vector-2d)) (dimmensions (make-vector-2d)) (color #(1 1 1 1)))
  (let* ((left (aref position 0))
		 (top (aref position 1))
		 (right (+ left (aref dimmensions 0)))
		 (bottom (+ top (aref dimmensions 1))))
	(gl:bind-texture :texture-2d sprite)
	(gl:color (aref color 0) (aref color 1) (aref color 2) (aref color 3))
	(gl:with-primitive :quads
	  (gl:tex-coord 0 1)
	  (gl:vertex left top 0)
	  (gl:tex-coord 1 1)
	  (gl:vertex  right top 0)
	  (gl:tex-coord 1 0)
	  (gl:vertex  right  bottom 0)
	  (gl:tex-coord 0 0)
	  (gl:vertex left bottom 0))))


(defun free-me-a-sprite (sprite)
  (gl:delete-textures (list sprite)))
  

;;; "ANIMACJA! ANIMACJA"
;;; (One of many polish demoscene "battle cries" ^^)

;;;; FIXME we could move caching to the texture level!!!!
(defparameter *animation-cache* nil "Cache for storing preloaded lists of textures.")

(defun clear-animation-cache ()
  "Clears cached animations. Called it at the beginning of the game, or else bad things will happen."
  (setf *animation-cache* (make-hash-table :test #'equalp)))

(defclass animation ()
  ((frames
	:initarg :frames
	:initform (error "Please specify animation frames"))
   (frame-time
	:initarg :frame-time
	:initform (error "Please specify time between frames."))
   (frames-left
	:initarg :frames-left
	:initform nil)
   (time-accumulator
	:initform 0)
   (loop-animation
	  :initarg :loop-animation
	  :initform t)))

(defmethod animation-end-p (animation)
  (with-slots (frames-left) animation
	(null frames-left)))

(defmethod update-animation (animation)
  (with-slots (frames frame-time frames-left time-accumulator loop-animation) animation
	(incf time-accumulator (sdl:dt))
	(if (> time-accumulator frame-time)
		(progn (setf frames-left (cdr frames-left))
			   (decf time-accumulator frame-time)))
	(if (and loop-animation (animation-end-p animation))
		(setf frames-left frames))))

(defmethod reset-animation (animation)
  (with-slots (frames frames-left time-accumulator) animation
	(setf frames-left frames)
	(setf time-accumulator 0)))

(defmethod set-to-random-frame (animation)
  (with-slots (frames frames-left time-accumulator) animation
	(setf frames-left (nthcdr (random (length frames)) frames))
	(setf time-accumulator 0)))

(defmethod draw-animation (animation &key position dimmensions (color #(1.0 1.0 1.0 1.0)))
  (draw-me-a-sprite (car (if (animation-end-p animation)
							 (slot-value animation 'frames)
							 (slot-value animation 'frames-left)))
					:position position
					:dimmensions dimmensions
					:color color))

(defun get-animation (name &optional (frame-time 0.05) (looping t))
  "Returns an animation object generated from image files in animations/name/ folder."

  ;;see if we can reuse textures from cache
  (aif (gethash name *animation-cache*)
	   (prog1 (make-instance 'animation :frames it :frame-time frame-time :frames-left it :loop-animation looping)
			  (format t "loading from cache!~%"))

	   ;;if not, load'em all
	   (flet ((get-picture-as-texture (image-name)
				(let ((the-image (sdl:load-image image-name :color-key-at #(0 0) :alpha 255)))
				  (prog1
					  (sdl-surface-to-opengl-texture the-image)
					(sdl:free the-image)))))
		 
		 (let* ((file-list (directory (concatenate 'string (namestring (full-pathname (concatenate 'string +animation-asset-path+ name))) "/*.*")))
				(frames (mapcar #'get-picture-as-texture file-list)))
		   
		   ;;FIXME color-key
		   (format t "loaded from file~%")
		   (setf (gethash name *animation-cache*) frames)
		   (make-instance 'animation :frames frames :frame-time frame-time :frames-left frames :loop-animation looping)))))


(defun drop-all-animations ()
  (maphash (lambda (k v)
			 (declare (ignore k))
			 (mapc #'free-me-a-sprite v))
		   *animation-cache*)
  (clear-animation-cache))
