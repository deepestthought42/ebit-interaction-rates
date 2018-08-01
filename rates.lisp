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
	 (destination (remove-if-not #'(lambda (index)
					 (and (= (a index) a)
					      (= (z index) z)
					      (= (q index) q)))
				     all-indices)))
    (cond
      ((not destination)
       (error "Couldn't find destination for: ~a" decay))
      (t (make-instance 'rate-of-change
			:fractional-rate-in-1/s (nubase:t1/2-to-decay-rates
						 (nubase:half-life decay))
			:description (nubase:name decay)
			:origin origin
			:destination (first destination))))))


(defun get-nuclear-decay-rates (origin decays all-indices)
  (labels ((filter ()
	     (remove-if-not #'(lambda (d)
				(and (= (a origin) (nubase:a d))
				     (= (z origin) (nubase:z d))))
			    decays :key #'nubase:mother)))
    (mapcar #'(lambda (d) (%create-rate-for-origin origin d all-indices))
	    (filter))))



(defun get-decay-rates-for-nuclides (nuclides maximum-lifetime)
  (let+ (((&slots nubase:decays nubase:nuclides)
	  (create-decays-for-nuclides nuclides maximum-lifetime))
	 (indices (get-indices-for-all-nuclides nubase:nuclides))
	 (n (length indices))
	 (nuclear-decays (alexandria:mappend
			  #'(lambda (index)
			      (get-nuclear-decay-rates index nubase:decays indices))
			  indices)))
    nuclear-decays))











