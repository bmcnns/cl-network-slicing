(in-package :cl-network-slicing)

(defun bit-rate (user)
  (/ (user-bits-transmitted user)
     (* *slot-duration* *slots-per-step*)))
  
(defun spectrum-efficiency (users)
  (/ (/ (reduce #'+ (mapcar #'user-bits-transmitted users))
	(* *slot-duration* *slots-per-step*))
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
  "Fraction of this step's arrivals in SLICE that met both SLAs.
   1.0 if nothing arrived."
  (flet ((on-time-p (packet)
           (let ((done (packet-transmission-complete packet)))
             (and done (<= done (packet-deadline packet))))))
    (if (zerop (slice-arrived slice))
        1d0
        (float (/ (loop for user in (slice-users slice)
                        sum (if (rate-sla-met-p user slice)
                                (count-if #'on-time-p (user-arrivals user))
                                0))
                  (slice-arrived slice))
               1d0))))

(defun utility (spectrum-efficiency volte-ssr video-ssr urllc-ssr)
  "Returns a value indicating how successful the network policy is."
  (+ (* 0.01d0 spectrum-efficiency)
     volte-ssr
     video-ssr
     urllc-ssr))
  
