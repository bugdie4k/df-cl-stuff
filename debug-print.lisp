(in-package #:df-cl-utils)

(defvar *dbp-count* 0)

(defun dbpe (&key (stream t) (increase-count? t) output-list)
  "extended"
  (labels ((%apply-autistic-formatting (list)
             (let ((prefix (format nil "~4A " (format nil "~A>" *dbp-count*))))
               (cons prefix (mapcar (lambda (el) (if (eq el :nl) (format nil "~%~A" prefix) (format nil "~A " el))) list)))))
    (when increase-count? (incf *dbp-count*))
    (format stream "~@[~{~A~}~]~%" (%apply-autistic-formatting output-list))))

(defun dbp (&rest output-list)
  "simple"
  (dbpe :stream t :increase-count? t :output-list output-list))

(defun dbps (&rest output-list-list)
  "several"
  (dbp (apply #'append output-list-list)))

(defun dbp-reset ()
  (setf *dbp-count* 0))
