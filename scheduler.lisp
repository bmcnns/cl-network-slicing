(in-package :cl-network-slicing)

(defun packet-expired-p (packet current-slot)
  "Returns T if the packet exceeds the deadline by tolerance amount."
  (let ((deadline-tolerance 0))
    (> (- current-slot (packet-deadline packet))
       deadline-tolerance)))

(defun next-packet (slice current-slot)
  "Choose which user and which packet gets served next."
  (let ((current-packet (slice-current-packet slice))
	(users (slice-users slice)))
    (when current-packet
      (if (packet-expired-p current-packet current-slot)
	  (progn
	    (incf (slice-dropped-timeout slice))
	    (setf (slice-current-packet slice) nil)
	    (dequeue (user-queue (find (packet-user-id current-packet) users :key #'user-id))) )
	  (return-from next-packet current-packet)))
    (loop repeat (length users)
	  for user = (elt users (slice-round-robin-pos slice))
	  do (setf (slice-round-robin-pos slice)
		   (mod (1+ (slice-round-robin-pos slice)) (length users)))
	  do (loop for packet = (queue-front (user-queue user))
		   while packet
		   if (packet-expired-p packet current-slot)
		     do (dequeue (user-queue user))
			(incf (slice-dropped-timeout slice))
		   else
		     do
			(setf (slice-current-packet slice) packet)
			(return-from next-packet packet)))))

	       
		 
	     
      
