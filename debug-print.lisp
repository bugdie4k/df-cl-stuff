(in-package #:df-cl-utils)

(defvar *dbp-count* 0)

(defun dbpe (&key (stream t) (increase-count? t) output-list)
  "extended"
  (labels ((%apply-autistic-formatting (list)
             (let ((prefix (format nil "~4A " (format nil "~A>" *dbp-count*))))
               (cons prefix (mapcar (lambda (el) (if (eq el :nl) (format nil "~%~A" prefix) (format nil "~A " el))) list)))))
    (format stream "~@[~{~A~}~]~%" (%apply-autistic-formatting output-list))
    (when increase-count? (incf *dbp-count*))))

(defun dbp (&rest output-list)
  "simple"
  (dbpe :stream t :increase-count? t :output-list output-list))

(defmacro dbps (&rest output-list-list)
  "several with implicit :nl"
  `(progn 
     ,@(mapcar (lambda (output-list) `(dbpe :stream t :increase-count? nil :output-list ',output-list)) (butlast output-list-list))
     (dbpe :stream t :increase-count? t :output-list ',(car (last output-list-list)))))

(defun dbp-reset ()
  (setf *dbp-count* 0))
