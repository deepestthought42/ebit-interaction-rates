;;;; package.lisp

(defpackage #:ebit-interaction-rates
  (:use #:cl #:iterate #:let-plus)
  (:export
   #:get-decay-rates
   #:ebit-system
   #:initial-populations
   #:current-density-in-A/cm^2
   #:velocity-electrons-cm/s
   #:trap-length-in-m
   #:beam-radius-in-um
   #:get-mass-numbers
   #:get-qVe
   #:get-qVt
   #:v{r_e}
   #:trap-depth-in-V
   #:initial-temperature-in-ev
   #:nuclide-indices
   #:spitzer-heating-constant
   #:exchange-constant
   #:get-q-ve-over-vol-x-kt
   #:collision-frequency-t-independent-ij
   #:pressure-in-mbar
   #:get-cx-rate-over-T-and-N
   #:source-terms
   #:get-source-rates)
  (:nicknames #:eir))

