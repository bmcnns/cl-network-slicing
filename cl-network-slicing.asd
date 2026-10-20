(asdf:defsystem "cl-network-slicing"
  :description "5G network slicing simulator in Common Lisp, built as a
native environment for evolving Tangled Program  Graph (TPG) agents."
  :version "0.1"
  :author "Bryce MacInnis, Xinyi Li"
  :components ((:file "package")
	       (:file "utils")
	       (:file "globals")
	       (:file "queue")
	       (:file "channel")
	       (:file "traffic")
	       (:file "user")
	       (:file "scheduler")
	       (:file "transmission")
	       (:file "environment")
	       (:file "main")))
