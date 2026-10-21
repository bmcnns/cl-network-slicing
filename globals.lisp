(in-package :cl-network-slicing)

(defparameter *network-bandwidth* 20.0d0
  "The amount of bandwidth in MHz")

(defparameter *base-station-power* (dbm->watts 30)
  "The amount of power supplied by the base station in watts.")

(defparameter *shadowing-sigma* 10d0
  "The standard deviation of shadowing (measured in dB)
   to simulate walls/objects blocking.")

(defparameter *slots-per-step* 400) 

(defparameter *slot-duration* 0.0005d0
  "The amount of seconds spent per slot.")

(defparameter *cell-radius* 40.0d0
  "Radius of the area of the base station in metres.")

(defparameter *users-per-slice*
  '((:volte . 46) (:video . 46) (:urllc . 8))
  "Number of users for each different network slice.")

(defparameter *maximum-queue-size* 5000
  "The total amount of packets allowed in a queue before
   not accepting more arrivals.")
  
(defparameter *num-steps* 100
  "The total amount of steps to run the simulator for.")
