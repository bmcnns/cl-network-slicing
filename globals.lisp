(in-package :cl-network-slicing)

(defparameter *network-bandwidth* 20.0d0
  "The amount of bandwidth in MHz")

(defparameter *slots-per-step* 4) 

(defparameter *min-distance* 35.0d0
  "Minimum distance of the users from the base station in metres.")

(defparameter *cell-radius* 40.0d0
  "Radius of the area of the base station in metres.")

(defparameter *users-per-slice*
  '((:volte . 1) (:video . 2) (:urllc . 3))
  "Number of users for each different network slice.")

(defparameter *maximum-queue-size* 5000
  "The total amount of packets allowed in a queue before
   not accepting more arrivals.")
  
