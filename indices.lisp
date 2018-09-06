(in-package #:ebit-interaction-rates)



(defun print-index (nuclide stream)
  (let+ (((&slots ebitodemessages:i ebitodemessages:A ebitodemessages:Z ebitodemessages:q) nuclide))
    (format stream "i:~D,A:~D,Z:~D,q:~D" ebitodemessages:i ebitodemessages:A ebitodemessages:Z ebitodemessages:q)))

(defun find-index (A Z q all-indices &key (errorp t) (on-error nil))
  (let ((nuclide (remove-if-not #'(lambda (nuclide)
				  (and (= (ebitodemessages:a nuclide) a)
				       (= (ebitodemessages:z nuclide) z)
				       (= (ebitodemessages:q nuclide) q)))
			      all-indices)))
    (if (not nuclide)
	(if errorp
	    (error "Couldn't find idestination for a: ~D, z: ~D, q: ~D" a z q)
	    on-error)
	(first nuclide))))


(defmethod print-object ((o ebitodemessages:nuclide) stream)
  (print-unreadable-object (o stream)
    (print-index o stream)))


(defun get-no-ion-species (nuclides)
  (iter
    (for n in nuclides)
    (sum (1+ (nubase:z n)))))


(defparameter *matrix-first-index* 1)


(defun get-indices-for-all-nuclides (nuclides &key (start-q 1))
  (iter outer
    (with index = *matrix-first-index*)
    (for nuclide in nuclides)
    (for A = (nubase:a nuclide))
    (for Z = (nubase:z nuclide))
    (iter
      (for q from start-q to Z)
      (in outer
	  (collect (make-instance 'ebitodemessages:nuclide :i index :a a :z z :q q)))
      (incf index))))



