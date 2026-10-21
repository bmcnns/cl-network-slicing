(in-package :cl-network-slicing)

(defstruct environment
  current-slot
  users
  slices
  (step 0))

(defun env-make ()
  (make-environment))

(defun env-reset (env)
  ;; reset current slot
  (setf (environment-current-slot env) 0)
  (setf (environment-step env) 0)
  
  ;; reset slices
  (setf (environment-slices env)
	(loop for (name . nil) in *users-per-slice*
	      collect (make-slice :name name)))

  ;; generate users
  (setf (environment-users env)
	(loop with id = -1
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
				      :queue (make-queue)
				      :next-arrival (sample-interarrival slice))))))

  ;; fill slices with users
  (dolist (slice (environment-slices env))
    (setf (slice-users slice)
	  (remove-if-not
	   (lambda (u) (eq (user-slice u) (slice-name slice)))
	   (environment-users env))))
  
  ;; throw away generate-packets once for statistical distribution
  ;; of first step to match subsequent
  (let ((counts (list :volte 0 :video 0 :urllc 0)))
    (dolist (user (environment-users env))
      (setf (user-next-arrival user)
	    (- (sample-interarrival (user-slice user)) *slots-per-step*))
      (incf (getf counts (user-slice user))
	    (length (generate-packets user (- *slots-per-step*)))))
    (values (loop for (name . nil) in *users-per-slice*
		  collect (getf counts name))
	    nil)))

(defun env-step (env action)
  (labels ((rescale-action (action)
	     ;; ensure actions are never negative
	     (let* ((no-negatives-action (mapcar (lambda (a-i) (max 0d0 a-i)) action))
		    (sum (reduce #'+ no-negatives-action))
		    (scaling-factor (if (> sum 0)
					(/ *network-bandwidth* sum)
					0)))
	       ;; ensure total allocation of bandwidth doesn't exceed network bandwidth
	       (if (> sum *network-bandwidth*)
		   (mapcar (lambda (item) (* item scaling-factor))
			   no-negatives-action)
		   no-negatives-action)))
	   (reset-step-counters (slice)
	     (setf (slice-arrived slice) 0)
	     (setf (slice-dropped-timeout slice) 0)
	     (setf (slice-dropped-overflow slice) 0)))

    ;; rescale action such that bandwidth is non-negative and
    ;; allocations are within total network bandwidth
    (setf action (rescale-action action))

    ;; apply bandwidth and power allocations
    (loop for (name . nil) in *users-per-slice*
	  for bw in action
	  for slice = (find name (environment-slices env) :key #'slice-name)
	  do
	     (setf (slice-bandwidth slice) (mhz->hz bw))
	     (setf (slice-power slice) (* *base-station-power* (/ bw *network-bandwidth*))))
    
    ;; reset per-step statistics
    (dolist (slice (environment-slices env))
      (reset-step-counters slice))

    ;; reset bits transmitted and arrivals for each user
    (dolist (user (environment-users env))
      (setf (user-bits-transmitted user) 0.0d0)
      (setf (user-arrivals user) nil))

    (let ((start (environment-current-slot env))
	  (buckets (make-array *slots-per-step*
			       :initial-element nil)))

      ;; generate traffic and assign to buckets
      (loop for user in (environment-users env)
	    for packets = (generate-packets user start)
	    do (dolist (packet packets)
		 (push packet
		       (aref buckets (- (packet-arrival-time packet)
					start)))))

      ;; simulate each slot
      (loop for i from 0 below *slots-per-step*
	    for slot = (+ start i)
	    for arrivals = (aref buckets i)
	    do (dolist (user (environment-users env))
		 (setf (user-fade user) (sample-exponential 1d0)))
	       (dolist (packet arrivals)
		 (let ((user (find (packet-user-id packet)
				   (environment-users env) :key #'user-id))
		       (slice (find (packet-slice packet)
				    (environment-slices env) :key #'slice-name)))
		   (push packet (user-arrivals user))
		   (incf (slice-arrived slice))
		   (if (< (queue-count (user-queue user)) *maximum-queue-size*)
		       (enqueue (user-queue user) packet)
		       (incf (slice-dropped-overflow slice)))))
	       (dolist (slice (environment-slices env))
		 (transmit-slice slice slot))
	       (incf (environment-current-slot env))))

    ;; increment step counter
    (incf (environment-step env))
    
    ;; obs, rew, term, trunc, info
    (let* ((obs (loop for (name . nil) in *users-per-slice*
		      for slice = (find name (environment-slices env) :key #'slice-name)
		      collect (slice-arrived slice)))
	   (efficiency (spectrum-efficiency (environment-users env)))
	   (volte-ssr (slice-success-rate (find :volte (environment-slices env) :key #'slice-name)))
	   (video-ssr (slice-success-rate (find :video (environment-slices env) :key #'slice-name)))
	   (urllc-ssr (slice-success-rate (find :urllc (environment-slices env) :key #'slice-name)))
	   (rew (utility efficiency volte-ssr video-ssr urllc-ssr))
	   (term (>= (environment-step env) *num-steps*)))
      (values obs rew term nil `(:volte-ssr ,volte-ssr :video-ssr ,video-ssr :urllc-ssr ,urllc-ssr)))))

