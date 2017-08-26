;;; Display constants
(defparameter +screen-width+ 800)
(defparameter +screen-height+ 600)
(defparameter +window-title+ "00:02 \"3D\" audio test")

;;; Simulation constants
(defparameter +fixed-dt+ (floor (/ 1000 30))) ; 30 steps / second
(defparameter +maximum-dt+ 500) ; maximum dt allowed - time step
                                ;will be trimmed to that value

;;; Resource constants
(defparameter +gfx-asset-path+ "data/gfx/")
(defparameter +sfx-asset-path+ "data/sfx/")
(defparameter +levels-asset-path+ "data/levels/")

;;; SOUND HANDLING

(defvar *buffer* nil)
(defvar *source* nil)
(defvar *data* nil)

;;; INPUT HANDLING
(defun mouse-pos-3d ()
  (let ((mouse-pos (sdl:mouse-position)))
	(vector (aref mouse-pos 0)
			(aref mouse-pos 1)
			0)))
;;; SOUND

(defun load-test-sound ()
  (multiple-value-bind (buffer source data)
	  (init-source-data #(0 0 0) #(0 0 0))
	(setq *buffer* buffer)
	(setq *source* source)
	(setq *data* data)))

(defun update-test-sound-pos (position)
  (al:source *source* :position
			 (scaled-vector (add-vectors position #(-400 -300 0)) (float (/ 1 100)))))

(defun init-listener ()
  (al:listener :position    #(0 0 0))
  (al:listener :velocity    #(0 0 0))
  (al:listener :orientation #(0.0  0.0 -1.0
                              0.0  1.0  0.0)))

(defun play-test-sound ()
  (al:source-play *source*))

(defun init-source-data (sourcepos sourcevel)
  (let ((buffer (al:gen-buffer))
        (source (al:gen-source))
        (data   (alut:load-memory-hello-world)))
    ;; AL:GET-ERROR somewhere about here
    ;; is generally a good idea.
    (al:buffer-data buffer :mono16 data 16356 11025)
    (al:source source :buffer buffer)
    (al:source source :pitch    1.0)
    (al:source source :gain     1.0)
    (al:source source :position sourcepos)
    (al:source source :velocity sourcevel)
    (al:source source :looping  t)
    ;; GET-ERROR to see this all went smooth.
    (values buffer source data)))

;;; MAIN FUNCTION

(defun run-test ()
  (alut:with-init
	  (sdl:with-init (sdl:SDL-INIT-VIDEO)
		(sdl:window +screen-width+ +screen-height+  ;screen resolution
					:title-caption +window-title+
					:fps (make-instance 'sdl:fps-timestep
										:max-dt +maximum-dt+ ; timestep upper bound
										:dt +fixed-dt+); fixed time step
					:video-driver "directx"
					:double-buffer t)
		
		(load-test-sound)
		(init-listener)
		(play-test-sound)
      ;;; EVENT PUMP / MAIN LOOP
		(sdl:with-events ()
		  ;; INPUT EVENTS
		  
		  ;; UPDATE EVENT
		  (:idle
		   (sdl:with-timestep ()
							  (update-test-sound-pos (mouse-pos-3d))
							  (print (mouse-pos-3d))
							  ))
		  ;; update game state manager here
		  ;;(update-gsm gsm (float (/ (sdl:dt) 1000)))) ; also convert dt from miliseconds to seconds
         ;; render game state manager
		  ;;(render-gsm gsm)
		  ;;(sdl:update-display))
		  
		  ;; QUIT EVENT
		  (:quit-event ()
					   (format t "quit event received~%")
					   t)))))