;;;; ebit-interaction-rates.asd

(asdf:defsystem #:ebit-interaction-rates
  :description "Describe ebit-interaction-rates here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :defsystem-depends-on  (#:cl-protobufs)
  :depends-on (#:iterate
		#:alexandria
		#:let-plus
		#:ionization-energies
		#:nubase
		#:ebit-cross-sections
		#:cl-protobufs
		#:group-by)
  :serial t
  :components ((:protobuf-file "ebit-interaction-rates-msgs/interaction-rate")
	       (:file "package")
	       (:file "constants")
	       (:file "indices")
	       (:file "rates")
	       (:file "ebit-system")
               (:file "ebit-interaction-rates")))

