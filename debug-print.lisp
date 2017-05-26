(in-package #:df-cl-utils)

(defvar *dbp-count* 0)

(defun dbp (&rest input)
  (labels ((%apply-autistic-formatting (list)
             (let ((prefix (format nil "~4A " (format nil "~A>" *dbp-count*))))
               (cons prefix (mapcar (lambda (el) (if (eq el :nl) (format nil "~%~A" prefix) (format nil "~A " el))) list)))))
    (incf *dbp-count*)
    (format t "~@[~{~A~}~]~%" (%apply-autistic-formatting input))))

(defun dbp-reset ()
  (setf *dbp-count* 0))
