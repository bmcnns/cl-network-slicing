(in-package :cl-network-slicing)

(defun path-loss (distance)
  "Calculate path loss using 3GPP Hotzone model
   PL(dB) = 140.7 + 36.7 * log10(d[km])"
  (let* ((path-loss-min-distance 10.0d0)
	 (distance (max distance path-loss-min-distance)))
    (+ 140.7d0 (* 36.7d0 (log (/ distance 1000.0d0) 10.0d0)))))

(defun channel-gain (distance shadowing-amount)
  "Calculate channel gain (linear scale)"
  (let ((antenna-gain 5d0))
    (expt 10.0d0 (- (/ (+ (- (path-loss distance) antenna-gain)
			  shadowing-amount)
		       10.0d0)))))

(defun signal-noise-ratio (gain power bandwidth)
  "Calculate signal-to-noise ratio.
   SNR = (g * P) / (N0 * BW)"
  (when (or (< bandwidth 0) (< power 0))
    (error "Can not receive negative bandwidth or negative power."))
  (when (or (= bandwidth 0) (= power 0))
    (return-from signal-noise-ratio 0d0))
  (let* ((thermal-noise -174) ;; dbm/hz
	 (noise-figure 9) ;; db
	 (n-0 (dbm->watts (+ thermal-noise noise-figure)))
	 (received-power (* gain power))
	 (noise-power (* n-0 bandwidth)))
    (/ received-power noise-power)))

(defun shannon-rate (gain power bandwidth)
  "Calculate Shannon capacity
   Rate = BW * log2(1 + SNR)"
  (* bandwidth (log (1+ (signal-noise-ratio gain power bandwidth)) 2)))

(defun transmittable-bits (gain power bandwidth duration)
  "How many bits can be transmitted in a given amount of seconds?"
  (* duration (shannon-rate gain power bandwidth)))

  
	

  
