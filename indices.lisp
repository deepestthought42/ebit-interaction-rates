(in-package #:ebit-interaction-rates)



(defun print-index (index stream)
  (let+ (((&slots ebitode:i ebitode:A ebitode:Z ebitode:q) index))
    (format stream "i:~D,A:~D,Z:~D,q:~D" ebitode:i ebitode:A ebitode:Z ebitode:q)))

(defun find-index (A Z q all-indices)
  (let ((index (remove-if-not #'(lambda (index)
				  (and (= (ebitode:a index) a)
				       (= (ebitode:z index) z)
				       (= (ebitode:q index) q)))
			      all-indices)))
    (if (not index)
	(error "Couldn't find idestination for a: ~D, z: ~D, q: ~D" a z q)
	(first index))))


(defmethod print-object ((o ebitode:index) stream)
  (print-unreadable-object (o stream)
    (print-index o stream)))


(defun get-no-ion-species (nuclides)
  (iter
    (for n in nuclides)
    (sum (1+ (nubase:z n)))))


(defparameter *matrix-first-index* 1)


(defun get-indices-for-all-nuclides (nuclides)
  (iter outer
    (with index = *matrix-first-index*)
    (for nuclide in nuclides)
    (for A = (nubase:a nuclide))
    (for Z = (nubase:z nuclide))
    (iter
      (for q from 0 to Z)
      (in outer
	  (collect (make-instance 'ebitode:index :i index :a a :z z :q q)))
      (incf index))))



