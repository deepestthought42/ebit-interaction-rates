;;;; package.lisp

(defpackage #:ebit-interaction-rates
  (:use #:cl #:iterate #:let-plus)
  (:export
   #:get-decay-rates
   #:ebit-system
   #:indices
   #:initial-populations)
  (:nicknames #:eir))

