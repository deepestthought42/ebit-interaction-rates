(in-package #:ebit-interaction-rates)



(defclass rate-of-change ()
  ((fractional-rate-in-1/s :initarg :fractional-rate-in-1/s :accessor fractional-rate-in-1/s 
			   :initform (error "Must initialize fractional-rate-in-1/s."))
   (description :initarg :description :accessor description 
		:initform (error "Must initialize description."))
   (origin :initarg :origin :accessor origin 
	   :initform (error "Must initialize origin."))
   (destination :initarg :destination :accessor destination 
		:initform (error "Must initialize destination."))))

(defmethod print-object ((o rate-of-change) stream)
  (print-unreadable-object (o stream)
    (format stream "R_{~a} = ~f/s [~a -> ~a]"
	    (description o) (fractional-rate-in-1/s o)
	    (print-index (origin o) nil)
	    (print-index (destination o) nil))))


(defun create-decays-for-nuclides (nuclides maximum-lifetime)
  (let+ ((decays (make-instance 'nubase:decays)))
    (iter
      (for n in nuclides)
      (nubase:create-decay-chains n :accumulator decays
				    :max-half-life maximum-lifetime))
    decays))



(defun %create-rate-for-origin (origin decay all-indices)
  (let+ ((z (nubase:z (nubase:daughter decay)))
	 (a (nubase:a (nubase:daughter decay)))
	 (q (min z (max 0 (+ (q origin)
			     (nubase:change-in-q decay)
			     (- z (z origin))))))
	 (destination (find-index a z q all-indices)))
    (make-instance 'rate-of-change
		   :fractional-rate-in-1/s (nubase:t1/2-to-decay-rates
					    (nubase:half-life decay))
		   :description (nubase:name decay)
		   :origin origin
		   :destination destination)))


(defun get-nuclear-decay-rates (origin decays all-indices)
  (labels ((filter ()
	     (remove-if-not #'(lambda (d)
				(and (= (a origin) (nubase:a d))
				     (= (z origin) (nubase:z d))))
			    decays :key #'nubase:mother)))
    (mapcar #'(lambda (d) (%create-rate-for-origin origin d all-indices))
	    (filter))))




(defun get-rr-rates (indices sigma-to-rate-factor electron-beam-energy-in-ev)
  (iter
    (for index in indices)
    (if (<= (q index) 0)
	(next-iteration))
    (collect
	(make-instance 'rate-of-change
		       :description "Radiative recombination"
		       :origin index
		       :destination (find-index (a index)
						(z index)
						(1- (q index))
						indices)
		       :fractional-rate-in-1/s
		       (* sigma-to-rate-factor
			  (cross:rr-cross-section electron-beam-energy-in-ev
						  (q index) (Z index)))))))

(defun get-ei-rates (indices sigma-to-rate-factor electron-beam-energy-in-ev)
  (iter
    (for index in indices)
    (if (>= (q index) (z index))
	(next-iteration))
    (collect
	(make-instance 'rate-of-change
		       :description "Electron impact"
		       :origin index
		       :destination (find-index (a index)
						(z index)
						(1+ (q index))
						indices)
		       :fractional-rate-in-1/s
		       (* sigma-to-rate-factor
			  (cross:rr-cross-section electron-beam-energy-in-ev
						  (q index) (Z index)))))))


(defun get-decay-rates-for-nuclides (nuclides maximum-lifetime
				     velocity-electrons-cm/s electron-rate
				     electron-beam-energy-in-ev)
  (let+ (;(ve-in-c (/ velocity-electrons-cm/s *c-in-cm/s*))
	 (sigma-to-rate-factor (* velocity-electrons-cm/s electron-rate))
	 ((&slots nubase:decays nubase:nuclides)
	  (create-decays-for-nuclides nuclides maximum-lifetime))
	 (indices (get-indices-for-all-nuclides nubase:nuclides))
	 ;(n (length indices))
	 (nuclear-decay-rates (alexandria:mappend
			  #'(lambda (index)
			      (get-nuclear-decay-rates index nubase:decays indices))
			  indices))
	 (rr-rates (get-rr-rates indices sigma-to-rate-factor electron-beam-energy-in-ev))
	 (ei-rates (get-ei-rates indices sigma-to-rate-factor electron-beam-energy-in-ev)))
    (sort (append nuclear-decay-rates rr-rates ei-rates)
	  #'< :key #'(lambda (r) (i (destination r))))))






#+nil
(get-decay-rates-for-nuclides (list (nubase:get-entry-for 20 8)) nubase:*stable*
			      1d0 1d0 1d0)


