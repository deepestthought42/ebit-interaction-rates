(in-package #:ebit-interaction-rates)



(defun %map-index-array (indices fun-of-a/z/q
			 &key (ret (make-array (length indices)
					       :element-type 'double-float))
			      (start-index 1))
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
			  (/ (* 1.569d15 (funcall fn-e-coulomb-log a z q)
				eir:current-density-in-A/cm^2
				q q)
			     (* eir:velocity-electrons-cm/s
				eir:velocity-electrons-cm/s
				a))))))



(defun get-mass-numbers (indices)
  (%map-index-array indices #'(lambda (a z q) (declare (ignore z q))
				(coerce a 'double-float))))


(defun get-qVe (indices Ve)
  (%map-index-array indices #'(lambda (a z q) (declare (ignore z a)) (* q Ve))))

(defun get-qVt (indices Vt)
  (%map-index-array indices #'(lambda (a z q) (declare (ignore z a)) (* q Vt))))

(defun collision-frequency (indices &key (coulomb-log-i/j (constantly 10d0)))
  (iter outer
    (for ind-i in-sequence indices)
    (iter
      (for ind-j in-sequence indices)
      (for i = (ebitodemessages:i ind-i))
      (for j = (ebitodemessages:i ind-j))
      (for q-i = (ebitodemessages:q ind-i))
      (for q-j = (ebitodemessages:q ind-j))
      (for A-i = (ebitodemessages:a ind-i))
      (for A-j = (ebitodemessages:a ind-j))
      (in outer
	  (collect (make-instance 'ebitodemessages:matrix-value
				  :row i :column j
				  :value (* 2.4937d-11 (/ (* q-i q-i q-j q-j
							     (funcall coulomb-log-i/j
								      q-i q-j A-i A-j))
							  (* A-i A-j)))))))))












