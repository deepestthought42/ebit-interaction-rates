(in-package #:ebit-interaction-rates)



(defun %map-index-array (indices fun-of-a/z/q
			 &key (ret (make-array (length indices)
					       :element-type 'double-float))
			      (start-index *start-index*))
  (let+ ((dim (length indices)))
    (iter
      (for index in-sequence indices)
      (for i = (ebitodemessages:i index))
      (if (>= i (+ dim start-index))
	  (error "indices are not spaced evenly ?"))
      (setf (aref ret (- i start-index))
	    (funcall fun-of-a/z/q
		     (ebitodemessages:a index)
		     (ebitodemessages:z index)
		     (ebitodemessages:q index))))
    ret))


(defun spitzer-heating-constant (system indices &key (fn-e-coulomb-log (constantly 10d0)))
  (let+ (((&slots eir:velocity-electrons-cm/s
		  eir:current-density-in-A/cm^2)
	  system))
    (%map-index-array indices
		      #'(lambda (a z q)
			  (/ (* 1.4616d18 (funcall fn-e-coulomb-log a z q) eir:current-density-in-A/cm^2 q q)
			     (* eir:velocity-electrons-cm/s eir:velocity-electrons-cm/s a))))))



(defun get-mass-numbers (indices)
  (%map-index-array indices #'(lambda (a z q) (declare (ignore z q))
				(coerce a 'double-float))))


(defun get-qVe (indices Ve)
  (%map-index-array indices #'(lambda (a z q) (declare (ignore z a)) (* q Ve))))

(defun get-qVt (indices Vt)
  (%map-index-array indices #'(lambda (a z q) (declare (ignore z a)) (* q Vt))))


(defun get-q-ve-over-vol-x-kt (indices Ve re-in-m trap-length-in-m)
  (%map-index-array indices
		    #'(lambda (a z q)
			(declare (ignore z a))
			(/ (* q Ve) (* pi trap-length-in-m
				       re-in-m re-in-m)))))



(defun collision-frequency-t-independent-ij (nuclides &key (coulomb-log-i/j (constantly 10d0)))
  "Given a list of ebitodemessages:nuclides in NUCLIDES, returns a list of items of type
ebitodemessages:matrix-value holding the temperature independent factor of the "
  (iter outer
    (for i in-sequence nuclides)
    (iter
      (for j in-sequence nuclides)
      (for val =
	   (make-instance 'ebitodemessages:matrix-value
			  :row (ebitodemessages:i i) :column (ebitodemessages:i j)
			  :value (* 1.49d-13
				    (/ (* (ebitodemessages:q i) (ebitodemessages:q i)
					  (ebitodemessages:q j) (ebitodemessages:q j)
					  (funcall coulomb-log-i/j
						   (ebitodemessages:q i)
						   (ebitodemessages:q j)
						   (ebitodemessages:A i)
						   (ebitodemessages:A j)))
				       (* (ebitodemessages:A i) (ebitodemessages:A j))))))
      (in outer (collect val)))))












