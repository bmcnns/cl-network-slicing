(in-package :cl-network-slicing)

(defvar *listener*)
(defvar *threads* '())
(defvar *env* nil)

(defun handle-connection (conn)
  (let ((stream (usocket:socket-stream conn)))
    (unwind-protect 
	(handler-case
	    (loop for line = (read-line stream nil nil)
		while line
		  do (handler-case 
			 (let* ((message (json:decode-json-from-string line))
				(command (cdr (assoc :command message))))
			   (cond
			     ((string= command "spaces")
			      (format t "SPACES~%")
			      (write-line
			       (json:encode-json-alist-to-string `((:num-observations . 3)
								   (:num-actions . 3)))
			       stream)
			      (finish-output stream))

			     ((string= command "make")
			      (progn
				(format t "ENV-MAKE~%")
				(setf *env* (env-make))
				(write-line 
				 (json:encode-json-alist-to-string '((:success . t)))
				 stream)
				(finish-output stream)))

			     ((string= command "reset")
			      (progn
				(format t "ENV-RESET~%")
				(multiple-value-bind (obs info)
				    (env-reset *env* :seed (cdr (assoc :seed message)))
				  (write-line
				   (json:encode-json-alist-to-string `((:obs . ,obs) (:info . ,info)))
				   stream))
				(finish-output stream)))

			     ((string= command "step")
			      (progn
				(format t "ENV-STEP~%")
				(multiple-value-bind (obs rew term trunc info)
				    (env-step *env* (cdr (assoc :action message)))
				  (write-line
				   (json:encode-json-alist-to-string
				    `((:obs . ,obs)
				      (:rew . ,rew)
				      (:term . ,term)
				      (:trunc . ,trunc)
				      (:info . ,info)))
				   stream)
				  (finish-output stream))))

			     ((string= command "close")
			      (progn
				(format t "STOP~%")
				(write-line
				 (json:encode-json-alist-to-string
				  `((:success . t)))
				 stream)
				(finish-output stream)))))
		       (error ()
			 (format t "Decoding message failed.~%"))))
	(usocket:socket-error ()
	  (return-from handle-connection)))
      (ignore-errors (usocket:socket-close conn)))))

(defun start-server (&key (host "127.0.0.1") (port 4762))
  "Starts listening on host:port for clients."
  (setf *listener*
	(usocket:socket-listen host port
			       :reuse-address t
			       :element-type 'character))
  (push (bt:make-thread
	 (lambda ()
	   (handler-case 
	       (loop for conn = (usocket:socket-accept *listener*)
		     do (handle-connection conn))
	     (usocket:socket-error ()
	       nil
	     )))
	 :name "tcp-listener")

	*threads*))

(defun stop-server ()
  (usocket:socket-close *listener*)
  (setf *env* nil)
  (setf *listener* nil)
  (setf *threads* nil))
  

  
		    
  
  
  
