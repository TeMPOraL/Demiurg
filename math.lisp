(defun square (x)
  (* x x))

(defun make-vector-2d (&optional (a 0) (b 0))
  "Creates a new 2D vector"
  (vector a b))

(defun add-vectors (vec1 vec2)
  "Sums two vectors."
  (map 'vector #'+ vec1 vec2))

(defun add-to-vector (vec1 vec2)
  "Sums two vectors into the first one"
  (map-into vec1 #'+ vec1 vec2))

(defun scaled-vector (vec1 scale)
  "Returns a new vector - vec1 scaled by given scalar"
  (map 'vector (lambda (elem) (* elem scale)) vec1))

(defun scale-vector (vec1 scale)
  "Scales vector by given scalar."
  (map-into vec1 (lambda (elem) (* elem scale)) vec1))

(defun negative-vector (vec1)
  "Returns a new vector - negation of given one"
  (map 'vector #'- vec1))
(defun negate-vector (vec1)
  "Negates current vector."
  (map-into vec1 #'- vec1))

(defun distance-between-vectors (vec1 vec2)
  (sqrt (reduce #'+ (map 'vector (lambda (x y) (square (- x y))) vec1 vec2))))

(defun vector-value (vec1)
  (sqrt (reduce (lambda (total x) (+ total (square x))) vec1 :initial-value 0.0)))

(defun normalized-vector (vec1)
  (scaled-vector vec1 (/ 1.0 (vector-value vec1))))

(defun clamp (what a b)
  (if (< what a) a
      (if (> what b) b
          what)))


(defun point-in-a-box-p (pos box-x box-y box-w box-h)
  (let ((transformed-x (- (aref pos 0) box-x))
		(transformed-y (- (aref pos 1) box-y)))
	(and (> transformed-x 0)
		 (> transformed-y 0)
		 (< transformed-x box-w)
		 (< transformed-y box-h))))


(declaim (inline rising-edge-p falling-edge-p))

(defun rising-edge-p (prev now)
  "Returns t if signal goes from nil to t."
  (and (not prev) now))

(defun falling-edge-p (prev now)
  "Returns t if signal goes from t to nil."
  (and prev (not now)))

(defun lerp (a b p)
  "Linear interpolation between a and b using parameter p [0..1]."
  (+ (* a (- 1 p))
	 (* b p)))