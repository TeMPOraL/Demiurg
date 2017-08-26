;;;; MAIN FILE FOR 00:02 GAME

;;; FIXME find a way to autoload the following libraries:
;;; - SDL
;;; - SDL_gfx
;;; - SDL_image
;;; - OpenAL (with ALUT)

;;; Display constants
(defparameter +screen-width+ 800)
(defparameter +screen-height+ 600)
(defparameter +window-title+ "00:02")

;;; Simulation constants
(defparameter +fixed-dt+ (floor (/ 1000 30))) ; 30 steps / second
(defparameter +maximum-dt+ 500) ; maximum dt allowed - time step
                                ;will be trimmed to that value

;;; Resource constants
;;(defparameter +gfx-asset-path+ "C:/TRC/na tapecie/krakjam/lisptest/00-02/data/gfx/")
(defparameter +gfx-asset-path+ "data/gfx/")
(defparameter +animation-asset-path+ "data/gfx/animations/")
(defparameter +sfx-asset-path+ "data/sfx/")
(defparameter +levels-asset-path+ "data/levels/")

;;; FIXME find a (better) way to load the files below
(load "macros.lisp")
(load "math.lisp")
(load "pictures.lisp")
(load "input.lisp")
(load "sound.lisp")
(load "game.lisp")
(load "events.lisp")

(load "location-manager.lisp")
(load "location.lisp")
(load "location-A.lisp")
(load "location-B.lisp")
(load "location-C1.lisp")
(load "location-C2.lisp")
(load "location-C3.lisp")
(load "location-C4.lisp")
(load "location-intro.lisp")

(defun init-opengl ()
  (gl:clear-color 1 0 0 0)
  ;; Initialize viewing values.
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 800 0 600 -1 1)
  (gl:enable :texture-2d)
  
  ;; Enable additive blending
  (gl:enable :blend)
;;  (gl:disable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha))


(defun run-game ()
  (alut:with-init
	  (sdl:with-init (sdl:SDL-INIT-VIDEO)
		(sdl:window +screen-width+ +screen-height+  ;screen resolution
					:title-caption +window-title+
					:fps (make-instance 'sdl:fps-timestep
										:max-dt +maximum-dt+ ; timestep upper bound
										:dt +fixed-dt+); fixed time step
					:video-driver "directx"
					:opengl t
					:opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
		
	    ;;; INIT HERE
;;		(sdl:show-cursor nil) TODO use this to display custom (hopefully animated) cursor.

		(init-opengl)


		(sdl-image:init-image :jpg :png :tif)
		(clear-animation-cache)

		(init-game)

		(let ((music (load-me-some-sound "music.wav")))

		  (play-me-a-sound music)
		  (setup-with-location :intro)
		  
      ;;; EVENT PUMP / MAIN LOOP
		(sdl:with-events ()
		  ;; INPUT EVENTS
		  
		  ;; UPDATE EVENT
		  (:idle
		   (gl:clear :color-buffer-bit)
		   (sdl:with-timestep ()
							  ;; UPDATE
							  (handle-input-dispatching)
							  (update-game-world)
							  )
		  ;; update game state manager here
		  ;;(update-gsm gsm (float (/ (sdl:dt) 1000)))) ; also convert dt from miliseconds to seconds
         ;; render game state manager
		  ;;(render-gsm gsm)

		   (draw-game-world)
		   (draw-inventory-bar)
		   (gl:flush)
		   (sdl:update-display))
		  

		  ;; QUIT EVENT
		  (:quit-event ()
					   (format t "quit event received~%")
					   (clear-event-data)
					   (clear-animation-cache)
					   t))))))