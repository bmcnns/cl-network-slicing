(in-package :cl-network-slicing)

(defstruct environment
  current-slot
  users
  slices)

(defun env-make ()
  (make-environment))

(defun env-reset (env)
  ;; reset current slot
  (setf (environment-current-slot env) 0)
  
  ;; reset slices
  (setf (environment-slices env)
	(loop for (name . nil) in *users-per-slice*
	      collect (make-slice :name name)))

  ;; generate users
  (setf (environment-users env)
	(loop with id = -12
	      for (slice . num-users) in *users-per-slice*
	      append (loop repeat num-users
			   collect (multiple-value-bind (x y gain)
				       (random-position-and-gain)
				     (make-user
				      :id (incf id)
				      :slice slice
				      :x x
				      :y y
				      :channel-gain gain
				      :queue (make-queue))))))

  ;; fill slices with users
  (dolist (slice (environment-slices env))
    (setf (slice-users slice)
	  (remove-if-not
	   (lambda (u) (eq (user-slice u) (slice-name slice)))
	   (environment-users env))))
  
  (values '(0 0 0) nil))

(defun env-step (env action)
  (labels ((rescale-action (action)
	     ;; ensure actions are never negative
	     (let* ((no-negatives-action (mapcar (lambda (a-i) (max 0d0 a-i)) action))
		    (sum (reduce #'+ no-negatives-action))
		    (scaling-factor (/ *network-bandwidth* sum)))
	       ;; ensure total allocation of bandwidth doesn't exceed network bandwidth
	       (if (> sum *network-bandwidth*)
		   (mapcar (lambda (item) (* item scaling-factor))
			   no-negatives-action)
		   no-negatives-action)))
	    (mhz->hz (n)
	     (* 1d6 n)))

    ;; rescale action such that bandwidth is non-negative and
    ;; allocations are within total network bandwidth
    (setf action (rescale-action action))

    ;; apply bandwidth allocations
    (loop for (name . nil) in *users-per-slice*
	  for bw in action
	  for slice = (find name (environment-slices env) :key #'slice-name)
	  do (setf (slice-bandwidth slice) (mhz->hz bw)))
    
    ;; generate traffic
    (loop for user in (environment-users env)
	  for slot = (environment-current-slot env)
	  do (dolist (packet (generate-packets user slot))
	       (when (< (queue-count (user-queue user))
			*maximum-queue-size*)
		 (enqueue (user-queue user) packet))))
    action))
  
