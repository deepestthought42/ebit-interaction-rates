;;;; ebit-interaction-rates.asd

(asdf:defsystem #:ebit-interaction-rates
  :description "Calculate interaction rates for EBIT ODEs."
  :author "Renee Klawitter <klawitterrenee@gmail.com>"
  :license  "Apache 2.0"
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
  :components ((:protobuf-file "ebit-ode-msg/ebit-ode-messages")
	       (:file "package")
	       (:file "constants")
	       (:file "indices")
	       (:file "rates")
	       (:file "ebit-system")
               (:file "ebit-interaction-rates")))

