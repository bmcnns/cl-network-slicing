(in-package :cl-network-slicing)

(defun random-position-and-gain ()
  (let ((theta (random (* 2 pi)))
	(radius (* *cell-radius* (sqrt (random 1.0d0))))
	(shadowing-db (sample-normal 0d0 *shadowing-sigma*)))
    (values (* radius (cos theta)) (* radius (sin theta))
	    (channel-gain radius shadowing-db))))

(defstruct user
  id
  slice
  x
  y
  channel-gain
  queue
  (bits-transmitted 0.0d0)
  arrivals
  next-arrival
  fade)
