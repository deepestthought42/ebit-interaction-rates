(in-package #:ebit-interaction-rates)



(defun print-index (index stream)
  (let+ (((&slots i A Z q) index))
    (format stream "i:~D,A:~D,Z:~D,q:~D" i A Z q)))

(defun find-index (A Z q all-indices)
  (let ((index (remove-if-not #'(lambda (index)
				  (and (= (a index) a)
				       (= (z index) z)
				       (= (q index) q)))
			      all-indices)))
    (if (not index)
	(error "Couldn't find idestination for a: ~D, z: ~D, q: ~D" a z q)
	(first index))))


(defmethod print-object ((o index) stream)
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
	  (collect (make-instance 'index :i index :a a :z z :q q)))
      (incf index))))



