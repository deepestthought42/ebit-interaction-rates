(in-package #:ebit-interaction-rates)



(defmethod print-object ((o ebitodemessages:matrix-value) stream)
  (print-unreadable-object (o stream)
    (format stream "R_{~D,~D} = ~f"
	    (ebitodemessages:row o) 
	    (ebitodemessages:column o) 
	    (ebitodemessages:value o))))


(defun create-decays-for-nuclides (nuclides maximum-lifetime)
  (let+ ((decays (make-instance 'nubase:decays)))
    (iter
      (for n in nuclides)
      (nubase:create-decay-chains n :accumulator decays
				    :max-half-life maximum-lifetime))
    decays))



(defun %create-rates (indices value-and-destination)
  (iter outer
    (for index in indices)
    (for values-and-destination
	 = (funcall value-and-destination index))
    (for origin-i = (ebitodemessages:i index))
    (iter
      (for (destination-i . value) in (funcall value-and-destination index))
      (in outer
	  (collect
	      (make-instance 'ebitodemessages:matrix-value
			     :column origin-i
			     :row destination-i
			     :value value))))))

(defmacro iter-indices ((indices index-symbol) &body body)
  `(%create-rates ,indices
		  #'(lambda (,index-symbol)
		      ,@body)))


(defun get-nuclear-decay-rates (indices decays &key (start-q 1))
  (iter-indices (indices index)
    (let+ ((decays-from-index (remove-if-not
			       #'(lambda (d)
				   (and (= (ebitodemessages:a index) (nubase:a d))
					(= (ebitodemessages:z index) (nubase:z d))))
			       decays :key #'nubase:mother))
	   (origin-i (ebitodemessages:i index)))
      
      (iter
	(for decay-from-index in decays-from-index)
	(let+ ((z (nubase:z (nubase:daughter decay-from-index)))
	       (a (nubase:a (nubase:daughter decay-from-index)))
	       (q (min z (max start-q (+ (ebitodemessages:q index)
					 (nubase:change-in-q decay-from-index)
					 (- z (ebitodemessages:z index))))))
	       (destination-i (ebitodemessages:i (find-index a z q indices)))
	       (rate (nubase:t1/2-to-decay-rates (nubase:half-life decay-from-index))))
	  (appending
	   (list
	    (cons origin-i (- rate))
	    (cons destination-i rate))))))))


(defun get-rr-rates (indices sigma-to-rate-factor electron-beam-energy-in-ev &key (start-q 1))
  (labels ((calc (index)
	     (* sigma-to-rate-factor
		(cross:rr-cross-section electron-beam-energy-in-ev
					(ebitodemessages:q index) (ebitodemessages:Z index)))))
    (iter-indices (indices index)
      (cond
	((<= (ebitodemessages:q index) 0d0) nil)
	((= (ebitodemessages:q index) start-q)
	 (list
	  (cons (ebitodemessages:i index)
		(- (calc index)))))
	(t
	 (list
	  (cons (ebitodemessages:i index)
		(- (calc index)))
	  (cons (ebitodemessages:i
		 (find-index (ebitodemessages:a index)
			     (ebitodemessages:z index)
			     (1- (ebitodemessages:q index))
			     indices))
		(calc index))))))))

(defun get-ei-rates (indices sigma-to-rate-factor electron-beam-energy-in-ev &key (start-q 1))
  (labels ((calc (index)
	     (* sigma-to-rate-factor
		(cross:ionization-cross-section electron-beam-energy-in-ev
						(ebitodemessages:Z index)
						(ebitodemessages:q index)))))
    (iter-indices (indices index)
      (cond
	((>= (ebitodemessages:q index) (ebitodemessages:z index)) nil)
	((<= (ebitodemessages:q index) (1- start-q))
	 (list
	  (cons (ebitodemessages:i index) (- (calc index)))))
	(t
	 (list
	  (cons (ebitodemessages:i index) (- (calc index)))
	  (cons (ebitodemessages:i
		 (find-index (ebitodemessages:a index)
			     (ebitodemessages:z index)
			     (1+ (ebitodemessages:q index))
			     indices))
		(calc index))))))))


(defun get-source-rates (indices source-terms)
  (iter
    (with retval = (make-array (length indices) :element-type 'double-float))
    (for index in-sequence indices with-index i)
    (iter
      (for st in source-terms)
      (let+ (((&key a q z rate) st)
	     ((&slots ebitodemessages:q ebitodemessages:a ebitodemessages:z)
	      index))
	(if (and (= a ebitodemessages:a)
		 (= q ebitodemessages:q)
		 (= z ebitodemessages:z))
	    (setf (aref retval i) rate)
	    (setf (aref retval i) 0d0))))
    (finally (return retval))))


(defun get-decay-rates-for-nuclides (nuclides indices maximum-lifetime
				     velocity-electrons-cm/s electron-rate
				     electron-beam-energy-in-ev)
  (let+ ((sigma-to-rate-factor (* velocity-electrons-cm/s electron-rate))
	 ((&slots nubase:decays) (create-decays-for-nuclides nuclides maximum-lifetime))
	 (nuclear-decay-rates (get-nuclear-decay-rates indices nubase:decays) )
	 (rr-rates (get-rr-rates indices sigma-to-rate-factor electron-beam-energy-in-ev))
	 (ei-rates (get-ei-rates indices sigma-to-rate-factor electron-beam-energy-in-ev)))
    (append nuclear-decay-rates rr-rates ei-rates)))






(defun avg-velocity-over-sqrt-of-kB-T (A)
  "given the atomic mass number A calculates: => <v> / sqrt(kT) = sqrt( (8 * kT) / (pi *
m) ) / sqrt( kT )"
  (* *c-in-cm/s*
     (sqrt (* (/ 8d0 (* pi *nucleon-mass-in-ev* A))
	   ;; the 2/3 due to the fact that we keep track of 3/2kT for the ions
	   (/ 2d0 3d0)))))


(defun atomic-density-in-1/cm3 (atomic-temp-in-K pressure-in-mbar)
  "given the atomic temperatre in [K]: ATOMIC-TEMP-IN-K and the pressure in [mbar]:
PRESSURE-IN-MBAR, calculate
 => n_0 = N / V = P / (kB * T)"
  (let* ((kB-in-mbar*cm3/K (* *kB-in-J/K* 1d4)))
    (/ pressure-in-mbar
       (* kB-in-mbar*cm3/K atomic-temp-in-K))))


(defun mueller-salzborn-cross-section (I q &key (k 1))
  "for the first ionization potential I and the charge state Q, calculate the charge
exchange cross-section sigma_{Q,Q-K} in cm2 using the Müller Salzborn scaling formula:
sigma_i,i-k = A_k*q^{alpha*k}*I[eV]^{beta_k} cm2 [Müller, Salzborn 1977]"
  (let* ((A_k (aref *salzborn-A_k* (1- k)))
	 (alpha_k (aref *salzborn-alpha_k* (1- k)))
	 (beta_k (aref *salzborn-beta_k* (1- k))))
    (* A_k (expt q alpha_k) (expt I beta_k))))



(defun get-cx-rate-over-T-and-N (indices I pressure-in-mbar
				 &key (start-q 1) (k 1) (atomic-temp-in-K 300))
  (let ((n0 (* (atomic-density-in-1/cm3 atomic-temp-in-K pressure-in-mbar))))
    (labels ((calc (index)
	       (* n0
		  (mueller-salzborn-cross-section I (ebitodemessages:q index) :k k)
		  (avg-velocity-over-sqrt-of-kB-T (ebitodemessages:a index)))))
      (%create-rates indices
		     #'(lambda (index)
			 (cond
			   ((< (ebitodemessages:q index) start-q) nil)
			   ((= (ebitodemessages:q index) start-q)
			    (list
			     (cons (ebitodemessages:i index) (- (calc index)))))
			   (t
			    (list
			     (cons (ebitodemessages:i index) (- (calc index)))
			     (cons (ebitodemessages:i
				    (find-index (ebitodemessages:a index)
						(ebitodemessages:z index)
						(1- (ebitodemessages:q index))
						indices))
				   (calc index))))))))))
