;;;; ebit-interaction-rates.asd

(asdf:defsystem #:ebit-interaction-rates
  :description "Describe ebit-interaction-rates here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:iterate
               #:alexandria
               #:let-plus
               #:ionization-energies
               #:nubase
               #:ebit-cross-sections)
  :serial t
  :components ((:file "package")
	       (:file "constants")
	       (:file "ebit-system")
	       (:file "indices")
	       (:file "rates")
               (:file "ebit-interaction-rates")))

