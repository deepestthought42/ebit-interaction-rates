;;;; ebit-interaction-rates.lisp

(in-package #:ebit-interaction-rates)

;;; "ebit-interaction-rates" goes here. Hacks and glory await!


(defmethod get-decay-rates ((system ebit-system) &key)
  (let+ (((&slots nuclides decay-maximum-lifetime
		  velocity-electrons-cm/s electron-rate
		  electron-beam-energy-in-ev)
	  system))
    (make-instance 'rate-list
		   :rates
		   (get-decay-rates-for-nuclides nuclides decay-maximum-lifetime
						 velocity-electrons-cm/s electron-rate
						 electron-beam-energy-in-ev))))



(defmethod write-decay-rates-to-file ((system ebit-system) filename &key (if-exists :error))
  (let+ ((rate-list (get-decay-rates system)))
    (with-open-file (stream filename
			    :direction :output
			    :if-exists if-exists
			    :element-type '(unsigned-byte 8))
      (proto:serialize-object-to-stream rate-list 'rate-list :stream stream ))))
