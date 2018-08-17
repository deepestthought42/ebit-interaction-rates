(in-package #:ebit-interaction-rates)



(defmethod print-object ((o ebitode:rate) stream)
  (print-unreadable-object (o stream)
    (format stream "R_{~a} = ~f/s [~a -> ~a]"
	    (ebitode:description o) (ebitode:rate-in-hz o)
	    (print-index (ebitode:origin o) nil)
	    (print-index (ebitode:destination o) nil))))


(defun create-decays-for-nuclides (nuclides maximum-lifetime)
  (let+ ((decays (make-instance 'nubase:decays)))
    (iter
      (for n in nuclides)
      (nubase:create-decay-chains n :accumulator decays
				    :max-half-life maximum-lifetime))
    decays))



(defun %create-rate-for-origin (origin decay all-indices &key (start-q 1))
  (let+ ((z (nubase:z (nubase:daughter decay)))
	 (a (nubase:a (nubase:daughter decay)))
	 (q (min z (max start-q (+ (ebitode:q origin)
				   (nubase:change-in-q decay)
				   (- z (ebitode:z origin))))))
	 (destination (find-index a z q all-indices)))
    (make-instance 'ebitode:rate
		   :rate-in-hz (nubase:t1/2-to-decay-rates
					    (nubase:half-life decay))
		   :description (nubase:name decay)
		   :origin origin
		   :destination destination)))


(defun get-nuclear-decay-rates (origin decays all-indices)
  (labels ((filter ()
	     (remove-if-not #'(lambda (d)
				(and (= (ebitode:a origin) (nubase:a d))
				     (= (ebitode:z origin) (nubase:z d))))
			    decays :key #'nubase:mother)))
    (mapcar #'(lambda (d) (%create-rate-for-origin origin d all-indices))
	    (filter))))




(defun get-rr-rates (indices sigma-to-rate-factor electron-beam-energy-in-ev &key (start-q 1))
  (iter
    (for index in indices)
    (if (<= (ebitode:q index) start-q)
	(next-iteration))
    (collect
	(make-instance 'ebitode:rate
		       :description "Radiative recombination"
		       :origin index
		       :destination (find-index (ebitode:a index)
						(ebitode:z index)
						(1- (ebitode:q index))
						indices)
		       :rate-in-hz
		       (* sigma-to-rate-factor
			  (cross:rr-cross-section electron-beam-energy-in-ev
						  (ebitode:q index) (ebitode:Z index)))))))

(defun get-ei-rates (indices sigma-to-rate-factor electron-beam-energy-in-ev)
  (iter
    (for index in indices)
    (if (>= (ebitode:q index) (ebitode:z index))
	(next-iteration))
    (collect
	(make-instance 'ebitode:rate
		       :description "Electron impact"
		       :origin index
		       :destination (find-index (ebitode:a index)
						(ebitode:z index)
						(1+ (ebitode:q index))
						indices)
		       :rate-in-hz
		       (* sigma-to-rate-factor
			  (cross:ionization-cross-section electron-beam-energy-in-ev
							  (ebitode:Z index) (ebitode:q index)))))))


(defun get-decay-rates-for-nuclides (nuclides indices maximum-lifetime
				     velocity-electrons-cm/s electron-rate
				     electron-beam-energy-in-ev)
  (let+ (;(ve-in-c (/ velocity-electrons-cm/s *c-in-cm/s*))
	 (sigma-to-rate-factor (* velocity-electrons-cm/s electron-rate))
	 ((&slots nubase:decays nubase:nuclides)
	  (create-decays-for-nuclides nuclides maximum-lifetime))
	 ;(n (length indices))
	 (nuclear-decay-rates (alexandria:mappend
			  #'(lambda (index)
			      (get-nuclear-decay-rates index nubase:decays indices))
			  indices))
	 (rr-rates (get-rr-rates indices sigma-to-rate-factor electron-beam-energy-in-ev))
	 (ei-rates (get-ei-rates indices sigma-to-rate-factor electron-beam-energy-in-ev)))
    (sort (append nuclear-decay-rates rr-rates ei-rates)
	  #'< :key #'(lambda (r) (ebitode:i (ebitode:destination r))))))






#+nil
(get-decay-rates-for-nuclides (list (nubase:get-entry-for 20 8)) nubase:*stable*
			      1d0 1d0 1d0)


