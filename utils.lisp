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

(defun bits->bytes (n)
  (/ n 8))

(defun bits->kilobytes (n)
  (/ (bits->bytes n) 1000))

(defun bits->megabytes (n)
  (/ (bits->kilobytes n) 1000))

(defun bits->gigabytes (n)
  (/ (bits->megabytes n) 1000))
