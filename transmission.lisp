(in-package :cl-network-slicing)

(defun transmit-slice (slice slot)
  (let ((bandwidth (slice-bandwidth slice))
	(power (slice-power slice)))
    ; no bandwidth, therefore we can't do anything.
    (when (zerop bandwidth)
      (return-from transmit-slice))
    (let ((time-left *slot-duration*))
      (loop while (> time-left 0)
	    do (let ((packet (next-packet slice slot)))
		 (unless packet
		   (return-from transmit-slice))
		 (let* ((user (find (packet-user-id packet) (slice-users slice) :key #'user-id))
			(gain (* (user-channel-gain user) (user-fade user)))
			(rate (shannon-rate gain power bandwidth)))
		   (unless (packet-transmission-start packet)
		     (setf (packet-transmission-start packet) slot))
		   (let* ((remaining (packet-remaining packet))
			  (time-needed (/ remaining rate)))
		     (if (<= time-needed time-left)
			 (progn
			   (decf time-left time-needed)
			   (setf (packet-remaining packet) 0.0d0)
			   (incf (user-bits-transmitted user) remaining)
			   (setf (packet-transmission-complete packet) slot)
			   (dequeue (user-queue user))
			   (setf (slice-current-packet slice) nil))
			 (let ((sent (* rate time-left)))
			   (decf (packet-remaining packet) sent)
			   (incf (user-bits-transmitted user) sent)
			   (setf time-left 0.0d0))))))))))
  
  
  
  
