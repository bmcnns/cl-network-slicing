(in-package :cl-network-slicing)

(defun bit-rate (user)
  (/ (user-bits-transmitted user)
     (* *slot-duration* *slots-per-step*)))
  
(defun spectrum-efficiency (slices)
  (/ (loop for slice in slices
	for bandwidth = (slice-bandwidth slice)
	for power = (slice-power slice)
	sum (loop for user in (slice-users slice)
		  sum (shannon-rate (user-channel-gain user) power bandwidth)))
     (mhz->hz *network-bandwidth*)))
	
(defun rate-sla-met-p (user slice)
  (let* ((gain (user-channel-gain user))
	 (power (slice-power slice))
	 (bandwidth (slice-bandwidth slice))
	 (rate (shannon-rate gain power bandwidth)))
    (>= rate (ecase (slice-name slice)
	       (:volte 51000)
	       (:urllc 10000000)
	       (:video 100000000)))))
  
(defun slice-success-rate (slice)
  "Number of successful packets over number of packets that arrived.
   1.0 if nothing arrived."
  (if (zerop (slice-arrived slice))
      1d0
      (float (/ (slice-successful slice) (slice-arrived slice)) 1d0)))

(defun utility (spectrum-efficiency volte-ssr video-ssr urllc-ssr)
  "Returns a value indicating how successful the network policy is."
  (+ (* 0.01d0 spectrum-efficiency)
     volte-ssr
     video-ssr
     urllc-ssr))
  
