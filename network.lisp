(in-package :cl-network-slicing)

(defstruct packet
  user-id
  slice
  arrival-time
  size
  deadline
  transmission-start
  transmission-complete
  remaining)

(defstruct slice
  name
  users
  bandwidth
  power
  (round-robin-pos 0)
  current-packet
  (arrived 0)
  (successful 0)
  (dropped-timeout 0)
  (dropped-overflow 0))
  

