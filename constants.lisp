(in-package #:ebit-interaction-rates)

(defparameter *start-index* 1)

(defun determine-limits (&optional (nubase-file nubase:*nubase2016*)
				   ;; an ion with Z protons can have
				   ;; Z+1 charge states
				   (z-delta-for-max-ion-species 1))
  (iter
    (for entry in nubase-file)
    (maximize (nubase:z entry) into max-z)
    (maximize (nubase:a entry) into max-a)
    (sum (+ (nubase:z entry) z-delta-for-max-ion-species)
	 into max-no-ion-species)
    (finally (return (values max-a max-z max-no-ion-species)))))





(let+ (((&values max-a max-z max-ion-species) (determine-limits)))
  (defparameter *max-a* max-a)
  (defparameter *max-z* max-z)
  (defparameter *max-no-ion-species* (+ 2 max-ion-species)
    "No of possible ion species when considering all of nubase plus
    two additional entries to store default population and temperature."))



(defparameter *c-in-cm/s* 3.0e10 "Speed of light")
(defparameter *e-mass-in-ev* 5.11e5 "Electron mass in eV")
(defparameter *e-chg-in-C* 1.6e-19 "Electron charge")
(defparameter *ln-lambda-i-j* 10d0)
(defparameter *kB-in-J/K* 1.381d-23 "Boltzmann constant in J/K")
(defparameter *nucleon-mass-in-ev* 931.5d6)

(defparameter *salzborn-A_k* #(1.43d-12 1.08d-12 5.5d-14 3.57d-16))
(defparameter *salzborn-alpha_k* #(1.17d0 0.71d0 2.1d0 4.2d0))
(defparameter *salzborn-beta_k* #(-2.76d0 -2.8d0 -2.89d0 -3.03d0))
