(in-package :cl-network-slicing)

(defun path-loss (distance)
  "Calculate path loss using 3GPP Urban Macro model
   PL(dB) = 128.1 + 37.6 * log10(d[km])"
  (+ 128.1d0 (* 37.6d0 (log (/ distance 1000.0d0) 10.0d0))))

(defun channel-gain (distance)
  "Calculate channel gain (linear scale)"
  (expt 10.0d0 (- (/ (path-loss distance) 10.0d0))))

(defun signal-noise-ratio (gain power bandwidth)
  "Calculate signal-to-noise ratio.
   SNR = (g * P) / (N0 * BW)"
  (when (or (< bandwidth 0) (< power 0))
    (error "Can not receive negative bandwidth or negative power."))
  (let* ((n-0 (expt 10 (/ (- (+ -174 9) 30) 10)))
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

  
	

  
