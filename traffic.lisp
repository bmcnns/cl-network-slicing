(in-package :cl-network-slicing)



(defun sample-interarrival (slice-type)
  "Returns a number indicating how many slots until the next packet."
  (ecase slice-type
    (:volte (sample-uniform 0d0 320d0))
    (:video (sample-truncated-pareto 5 1.2d0 25))
    (:urllc (sample-exponential (/ 1d0 360d0)))))

(defun slice->packet-size (slice)
  "Given a slice, return the number of bits that a packet should be."
  (ecase slice
    (:volte 320.0d0)
    (:video (bytes->bits (sample-truncated-pareto 40 1.2d0 250)))
    (:urllc (kilobytes->bits (random-choice '(6.4d0 12.8d0 19.2d0 25.6d0 32d0))))))

(defun generate-packets (user start-slot)
  "Generates the packets that arrives for a user within a scheduling window."
  (let* ((slice (user-slice user)))
    (loop for slot = (user-next-arrival user)
	    then (+ slot (sample-interarrival slice))
	  for arrival = (floor slot)
	  while (< slot (+ start-slot *slots-per-step*))
	  collect (let ((packet-size (slice->packet-size slice)))
		    (make-packet
		     :user-id (user-id user)
		     :slice slice
		     :arrival-time arrival
		     :size packet-size
		     :deadline (+ arrival
				  (ecase slice
				    (:volte 20)
				    (:video 20)
				    (:urllc 2)))
		     :remaining packet-size))
	  finally (setf (user-next-arrival user) slot))))
  
			    
  
