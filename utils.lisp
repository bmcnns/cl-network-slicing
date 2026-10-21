(in-package :cl-network-slicing)

(defun random-choice (seq)
  (elt seq (random (length seq))))

(defun shuffle (list)
  "Return a new list with elements in random order."
  (let ((v (coerce list 'vector)))     ; fresh vector, so LIST is untouched
    (loop for i from (1- (length v)) downto 1
          for j = (random (1+ i))
          do (rotatef (aref v i) (aref v j)))
    (coerce v 'list)))

(defun dbm->watts (dbm)
  (expt 10d0 (/ (- dbm 30d0) 10d0)))

(defun bits->bytes (n)
  (/ n 8))

(defun bytes->bits (n)
  (* n 8))

(defun kilobytes->bits (n)
  (* (bytes->bits n) 1000))


(defun megabytes->bits (n)
  (* (bytes->bits n) 1000000))

(defun bits->kilobytes (n)
  (/ (bits->bytes n) 1000))

(defun bits->megabytes (n)
  (/ (bits->kilobytes n) 1000))

(defun bits->gigabytes (n)
  (/ (bits->megabytes n) 1000))

(defun mhz->hz (n)
  (* 1d6 n))

(defun sample-uniform (a b)
  "Returns a number uniformly sampled in [a,b)."
  (+ a (random (- b a))))

(defun sample-exponential (lambda)
  "Returns a number sampled from an exponential distribution parameterized by lambda."
  (/ (- (log (- 1d0 (random 1d0)))) lambda))

(defun sample-truncated-pareto (scale alpha max)
  (let* ((u (- 1d0 (random 1d0)))
	 (x (/ scale (expt u (/ 1 alpha)))))
    (min x max)))

(defun sample-normal (mu std)
  (let* ((u1 (- 1d0 (random 1d0)))
	 (u2 (random 1d0))
	 (z (* (sqrt (* -2 (log u1))) (cos (* 2 pi u2)))))
    (+ (* std z) mu)))
	
