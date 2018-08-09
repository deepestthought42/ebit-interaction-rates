(in-package #:ebit-interaction-rates)


(defclass ebit-system ()
  ((nuclides :initarg :nuclides :accessor nuclides 
	     :initform (error "Must initialize nuclides."))
   (initial-populations :accessor initial-populations
			:initarg :initial-populations :initform '())
   (source-terms :accessor source-terms :initarg :source-terms :initform '())
   (default-population :accessor default-population :initarg :default-population
		       :initform 0d0)
   (initial-temperature-in-ev :accessor initial-temperature-in-ev
			      :initarg :initial-temperature-in-ev
			      :initform 10d0)
   (default-ion-temperature-in-ev :accessor default-ion-temperature-in-ev
				  :initarg :default-ion-temperature-in-ev :initform 10d0)
   (beam-current-in-a :accessor beam-current-in-a :initarg :beam-current-in-a :initform 0.1d0)
   (beam-radius-in-um :accessor beam-radius-in-um :initarg :beam-radius-in-um
		      :initform 50d0)
   (electron-beam-energy-in-ev :accessor electron-beam-energy-in-ev
			       :initarg :electron-beam-energy-in-ev
			       :initform 5000d0)
   (n0-in-torr :accessor n0-in-torr :initarg :n0-in-torr :initform 1d-10)
   (current-density-in-A/cm^2 :accessor current-density-in-A/cm^2 :initform 1d0)
   (velocity-electrons-cm/s :accessor velocity-electrons-cm/s :initform 1d0)
   (trap-depth-in-V :accessor trap-depth-in-V :initarg :trap-depth-in-V :initform 25d0)
   (electron-rate :accessor electron-rate :initform 1d0)
   (trap-length-in-m :accessor trap-length-in-m :initarg :trap-length-in-m :initform 0.08d0)
   (v-0 :accessor v-0 :initarg :v-0 :initform 0d0)
   (v{r_e} :accessor v{r_e} :initarg :v{r_e} :initform 0d0)
   (decay-maximum-lifetime :accessor decay-maximum-lifetime :initarg
			   :decay-maximum-lifetime :initform nubase:*stable*)
   (accept-trap-depth-in-multiple-of-v-0 :accessor accept-trap-depth-in-multiple-of-v-0
					 :initarg :accept-trap-depth-in-multiple-of-v-0 :initform 5d0)))


(defun electron-velocity (kinetic-energy-in-ev)
  (let ((v/c (sqrt (- 1 (/ 1 (expt (+ 1 (/ kinetic-energy-in-ev *e-mass-in-ev*)) 2)))))
	(classical-v/c (* (sqrt (/ (* 2 kinetic-energy-in-ev) *e-mass-in-ev*)))))
    (values (* *c-in-cm/s* v/c) v/c classical-v/c)))


(defun calc-v-0 (I-in-a e-beam-energy-in-ev)
  (/ (* 30d0 I-in-a)
     (sqrt (- 1 (expt (+ (/ e-beam-energy-in-ev *e-mass-in-ev*) 1d0) -2d0)))))



(defun v-space-charge (r r-e I-in-a e-beam-energy-in-ev)
  (let ((v0 (- (calc-v-0 I-in-a e-beam-energy-in-ev)))
	(r (abs r)))
    (- (if (< r r-e)
	   (* v0 (expt (/ r r-e) 2))
	   (* v0 (+ 1 (* 2 (log (/ r r-e)))))))))



(defun create-decays-for-system (system)
  (let+ (((&slots nuclides decay-maximum-lifetime) system)
	 (decays (make-instance 'nubase:decays)))
    (iter
      (for n in nuclides)
      (nubase:create-decay-chains n :accumulator decays
				    :max-half-life decay-maximum-lifetime))
    decays))



(defmethod initialize-instance :after ((sys ebit-system) &key)
  (let+ (((&slots current-density-in-A/cm^2 beam-radius-in-um
		  nuclides
		  electron-beam-energy-in-ev beam-current-in-a
		  velocity-electrons-cm/s electron-rate
		  accept-trap-depth-in-multiple-of-v-0
		  trap-depth-in-V
		  accessories v-0 v{r_e})
	  sys)
	 (beam-radius-in-cm (* 1d-4 beam-radius-in-um))
	 (decays (create-decays-for-system sys)))
    (setf nuclides (nubase:nuclides decays)
	  current-density-in-a/cm^2 (/ beam-current-in-a (* pi beam-radius-in-cm beam-radius-in-cm))
	  velocity-electrons-cm/s (electron-velocity electron-beam-energy-in-ev)
	  electron-rate (/ current-density-in-a/cm^2 *e-chg-in-C* velocity-electrons-cm/s)
	  v{r_e} (v-space-charge beam-radius-in-um beam-radius-in-um
				 beam-current-in-a electron-beam-energy-in-ev)
	  v-0 (calc-v-0 beam-current-in-a electron-beam-energy-in-ev))
    (if (< (* accept-trap-depth-in-multiple-of-v-0 v{r_e}) trap-depth-in-V)
	(error "Using the given trap depth: ~,2fV, with the given settings 
leading to an electron beam potential of: ~,2fV, incurs large errors
on the overlap. Adjust ACCEPT-TRAP-DEPTH-IN-MULTIPLE-OF-V-0: ~,2f if necessary."
	       trap-depth-in-V v{r_e}
	       accept-trap-depth-in-multiple-of-v-0))))










