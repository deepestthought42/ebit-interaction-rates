
(in-package #:ebit-interaction-rates)


(defun create-decays-for-nuclides (source-nuclides
				   &key (include-decay-p #'nubase:include-decay-p)
					(include-nuclide-p (constantly t)))
  "Returns an object of type NUBASE:DECAYS."
  (let+ ((decays (make-instance 'nubase:decays)))
    (iter
      (for n in source-nuclides)
      (nubase:create-decay-chains n :accumulator decays
				    :include-decay-p include-decay-p
				    :include-nuclide-p include-nuclide-p))
    decays))





