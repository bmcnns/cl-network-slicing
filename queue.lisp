(in-package :cl-network-slicing)

(defstruct queue
  (head '())
  (tail nil)
  (size 0))

(defun enqueue (q item)
  (let ((new (cons item nil)))
    (if (queue-tail q)
	(setf (cdr (queue-tail q)) new)
	(setf (queue-head q) new))
    (setf (queue-tail q) new)
    (incf (queue-size q)))
  q)

(defun dequeue (q)
  (if (queue-head q)
      (let ((item (first (queue-head q))))
	(setf (queue-head q) (cdr (queue-head q)))
	(unless (queue-head q)
	  (setf (queue-tail q) nil))
	(decf (queue-size q))
	item)))
	

(defun queue-front (q)
  (first (queue-head q)))

(defun queue-count (q)
  (queue-size q))

(defun queue-empty-p (q)
  (= (queue-count q) 0))

	   
