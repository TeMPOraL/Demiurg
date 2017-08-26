;;;; GAME CLUTTER GOES HERE

(defparameter *inventory* () "Player's items.")
(defparameter *selected-item* () "Item that the player currently holds.")

(defconstant +inventory-bar-size+ (vector +screen-width+ 50))
(defconstant +inventory-bar-position+ (vector 0 (- +screen-height+ (aref +inventory-bar-size+ 1))))

(defconstant +inventory-drawing-offset-start+ 10 "Display margin for items from the beginning of the inventory.")
(defconstant +inventory-drawing-offset-increment+ 40 " = spacing between items + size of an item.")

(defconstant +item-size+ (vector 30 30))
(defconstant +item-drawing-offset-bottom+ 10)

(defparameter *game-initializers* nil "An ugly hack that reduces the amount of let clutter, but forces to recompile the whole file sometimes.")

;;; INVENTORY BAR
(let ((inventory-bar-pic nil))
  (push  (lambda ()
		   (setf inventory-bar-pic (make-me-a-sprite "UI_bar.jpg")))
		 *game-initializers*)
	

  (defun draw-inventory-bar ()
	"The utility function to draw inventory bar with items and all the UI effects - highlighting, etc."
	
	;;FIXME replace with a real image (or animation)
	(draw-me-a-sprite inventory-bar-pic
					  :position +inventory-bar-position+
					  :dimmensions +inventory-bar-size+
					  :color #(1.0 1.0 1.0 0.5))
	
	;;draw items from inventory
	(let ((offset-x +inventory-drawing-offset-start+))
	  (mapc (lambda (item)
			  (draw-item item offset-x)
			  (incf offset-x +inventory-drawing-offset-increment+))
			  *inventory*))))
  

;;; INVENTORY INTERFACE
(defun selected-item ()
  "Returns the item user has selected, or nil if he has none"
  *selected-item*)

(declaim (inline item-selected-p))
(defun item-selected-p (item)
  (eql item *selected-item*))

(defun remove-item (item)
  "Removes an item from the inventory and deselects it, if it was selected."
  (if (eql *selected-item* item)
	  (setq *selected-item* nil))
  (setq *inventory* (remove item *inventory*)))

(defun select-item (item)
  (setf *selected-item* item))

;;; INVENTORY OPERATIONS
(defun handle-inventory-click ()
  (mapc (lambda (item)
		  (if (mouse-over-item-p item)
			  (select-item item)))
		*inventory*))

;;; ITEM DRAWING & DISPLAY
;;; FIXME do it properly
(let ((default-item-sprite nil)
	  (clay-item-sprite nil)
	  (heart-item-sprite nil)
	  (glass-item-sprite nil))
  (defmethod draw-item (item offset) ;Default implementation
	(draw-me-a-sprite (case item
						(:clay clay-item-sprite)
						(:heart heart-item-sprite)
						(otherwise default-item-sprite))
					  :position (vector (+ (aref +inventory-bar-position+ 0) offset)
										(+ (aref +inventory-bar-position+ 1) +item-drawing-offset-bottom+))
					  :dimmensions +item-size+
					  :color (cond ((item-selected-p item) (vector 1 0 0 1))
								   ((mouse-over-item-p item) (vector 0 1 0 1))
								   (t (vector 1 1 1 1)))))
  (push (lambda ()
		  (setf default-item-sprite (make-me-a-sprite "item.png"))
		  (setf clay-item-sprite (make-me-a-sprite "UI_clay.png"))
		  (setf heart-item-sprite (make-me-a-sprite "UI_heart.png"))
		  (setf glass-item-sprite (make-me-a-sprite "item.png")))
		
		  *game-initializers* ))

(defun mouse-over-item-p (item)
  (aif (position item *inventory*)
	   (let ((x-offset (+ +inventory-drawing-offset-start+ (* it +inventory-drawing-offset-increment+))))
		 (point-in-a-box-p (mouse-position)
						   x-offset (+ (aref +inventory-bar-position+ 1) +item-drawing-offset-bottom+)
						   (aref +item-size+ 0) (aref +item-size+ 1)))))


;;; TO DO (TO BE MOVED SOMEWHERE WHEN DONE)
(defun handle-game-click (location)
  (format t "ojej ~a ~%" (mouse-position))
  (handle-mouse-click location))

(defun add-random-item ()
  (push (gensym) *inventory*))

(defun give-item (item)
  (push item *inventory*))


;;; INPUT HANDLING
(let ((lmb-last-pressed nil)) ;;NOTE hmm... I wonder whether there's a more elegant equivalent of this... ;)
  (defun dispatch-input (location)

	;;TODO handle keyboard input
	
	
	;;TODO handle mouse input
	(if (rising-edge-p lmb-last-pressed (sdl:mouse-left-p))
		(if (mouse-in-a-box-p (aref +inventory-bar-position+ 0)
							  (aref +inventory-bar-position+ 1)
							  (aref +inventory-bar-size+ 0)
							  (aref +inventory-bar-size+ 1))
			(handle-inventory-click)
			(handle-game-click location)))
	
	(setf lmb-last-pressed (sdl:mouse-left-p))))


;;; GAME - general
;;; NOTE this is a little hacky :(.
(defun init-game ()
  (mapc #'funcall *game-initializers*))