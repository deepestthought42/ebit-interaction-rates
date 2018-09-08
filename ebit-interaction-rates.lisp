;;;; ebit-interaction-rates.lisp

(in-package #:ebit-interaction-rates)

;;; "ebit-interaction-rates" goes here. Hacks and glory await!


(defmethod get-decay-rates ((system ebit-system) &key)
  (let+ (((&slots nuclides decay-maximum-lifetime
		  velocity-electrons-cm/s electron-rate
		  electron-beam-energy-in-ev nuclide-indices
		  source-terms)
	  system)
	 (rates (get-decay-rates-for-nuclides nuclides nuclide-indices
					      decay-maximum-lifetime
					      velocity-electrons-cm/s electron-rate
					      electron-beam-energy-in-ev))
	 (dimension (length (remove-duplicates rates :key #'(lambda (r) (ebitodemessages:row r))))))
    (values rates dimension)))

#+nil
(defmethod write-decay-rates-to-file ((system ebit-system) filename &key (if-exists :error))
  (let+ ((rate-list (get-decay-rates system)))
    (with-open-file (stream filename
			    :direction :output
			    :if-exists if-exists
			    :element-type '(unsigned-byte 8))
      (proto:serialize-object-to-stream rate-list 'ebitodemessages:rate-list :stream stream ))))


