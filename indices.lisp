(in-package #:ebit-interaction-rates)



(defclass index ()
  ((i :initarg :i :accessor i 
      :initform (error "Must initialize i."))
   (A :initarg :A :accessor A 
      :initform (error "Must initialize A."))
   (Z :initarg :Z :accessor Z 
      :initform (error "Must initialize Z."))
   (q :initarg :q :accessor q 
      :initform (error "Must initialize q."))))


(defun print-index (index stream)
  (let+ (((&slots i A Z q) index))
    (format stream "i:~D,A:~D,Z:~D,q:~D" i A Z q)))


(defmethod print-object ((o index) stream)
  (print-unreadable-object (o stream)
    (print-index o stream)))

(print-object )


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



