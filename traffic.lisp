(in-package :cl-network-slicing)

(defun sample-uniform (a b)
  "Returns a number uniformly sampled in [a,b)."
  (+ a (random (- b a))))

(defun sample-exponential (lambda)
  "Returns a number sampled from an exponential distribution parameterized by lambda."
  (/ (- (log (- 1d0 (random 1d0)))) lambda))

(defun sample-interarrival (slice-type)
  "Returns a number indicating how many slots until the next packet."
  (ecase slice-type
    (:volte (sample-uniform 1.0d0 3.0d0))
    (:video (min (sample-exponential 2.0d0) 40.0d0))
    (:urllc (sample-exponential 4.0d0))))

(defun generate-packets (user start-slot)
  "Generates the packets that arrives for a user within a scheduling window."
  (let* ((slice (user-slice user))
	 (packet-size (ecase slice
			(:volte 1020d0)
			(:urllc 500d0)
			(:video 1280d0)))
	 (deadline (ecase slice
		     (:volte (+ start-slot 40.0d0))
		     (:urllc (+ start-slot 30.0d0))
		     (:video (+ start-slot 40.0d0)))))
    (loop for slot = (+ start-slot (sample-interarrival slice))
	    then (+ slot (sample-interarrival slice))
	  while (< slot (+ start-slot *slots-per-step*))
	  collect (make-packet
		   :user-id (user-id user)
		   :slice slice
		   :arrival-time (floor slot)
		   :size packet-size
		   :deadline deadline
		   :remaining packet-size))))
		   
			    
  
