;;;; ebit-interaction-rates.lisp

(in-package #:ebit-interaction-rates)

;;; "ebit-interaction-rates" goes here. Hacks and glory await!


(defmethod get-decay-rates ((system ebit-system) &key)
  (let+ (((&slots nuclides decay-maximum-lifetime
		  velocity-electrons-cm/s electron-rate
		  electron-beam-energy-in-ev indices)
	  system)
	 (rates (get-decay-rates-for-nuclides nuclides indices
					      decay-maximum-lifetime
					      velocity-electrons-cm/s electron-rate
					      electron-beam-energy-in-ev))
	 (dimension (length (remove-duplicates rates :key #'(lambda (r) (ebitode:i (ebitode:destination r)))))))
    (make-instance 'ebitode:rate-list
		   :rates rates
		   :dimension dimension)))


(defmethod write-decay-rates-to-file ((system ebit-system) filename &key (if-exists :error))
  (let+ ((rate-list (get-decay-rates system)))
    (with-open-file (stream filename
			    :direction :output
			    :if-exists if-exists
			    :element-type '(unsigned-byte 8))
      (proto:serialize-object-to-stream rate-list 'ebitode:rate-list :stream stream ))))


