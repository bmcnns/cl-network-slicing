(in-package :cl-network-slicing)

(defun random-position-and-gain ()
  (let ((theta (random (* 2 pi)))
	(radius (+ *min-distance* (random (- *cell-radius* *min-distance*)))))
    (values (* radius (cos theta)) (* radius (sin theta)) (channel-gain radius))))

(defstruct user
  id
  slice
  x
  y
  channel-gain
  queue)
