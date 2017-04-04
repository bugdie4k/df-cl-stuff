(in-package #:my-cl-utils)

(defun replace-in-tree (lst &key (old (error "Supply OLD argument to REPLACE-IN-TREE function"))
                                 (new (error "Supply NEW argument to REPLACE-IN-TREE function"))
                                 (test-fn #'equalp))
  "Traverses list as a tree and replaces old value with the new one.
Uses test-fn to compare old with elements of lists to decide if this is a value to replace."
  (labels ((%replace (lst)
             (when lst
               (let ((1st (first lst)))
                 (cons
                  (if (listp 1st)
                      (%replace 1st)
                      (if (funcall test-fn 1st old) new 1st))
                  (%replace (rest lst)))))))
    (%replace lst)))

(defmacro mv-let* (mv-let-list &body body)
  "mv-let-list is a list of a form ((var1 var2 var3) (form-to-return-3-values)
                                    (var1 var2) (form-to-return-2-values) ...)"
  (labels ((%expand-mv-let* (mv-let-list)
             (let ((mv-let1 (pop mv-let-list)))
               (if mv-let-list
                   `(multiple-value-bind ,(first mv-let1) ,(second mv-let1)
                      ,(%expand-mv-let* mv-let-list))
                   `(multiple-value-bind ,(first mv-let1) ,(second mv-let1)
                     ,@body)))))
    (%expand-mv-let* mv-let-list)))

(defmacro format-concatenate (&rest format-lists)
  "format-lists is a list of format lists in which first el is format-control
and rest is format-arguments.
example: (format-concatenate (\"~A ~A\" a b) (\"---\")"
  `(concatenate 'string
                ,@(mapcar (lambda (format-list) `(format nil ,@format-list)) format-lists)))
