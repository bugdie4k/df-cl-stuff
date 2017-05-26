(in-package #:df-cl-utils)

(defun skip-el (el lst &key (test #'equalp))
  "Skips 'el' in 'lst' and returnes the part where a sequence of elements 'el' is over."
  (when lst
    (if (funcall test el (car lst))
        (skip-el el (cdr lst) :test test)
        lst)))

(defun string-equal-with-wildcards (&key string-with-wildcards string (wildcard #\*))
  "Compares two strings with consideration of wildcards in the first one.
I originally made this as a task for my SHARAGA, but I decided to keep it, whatever."
  (labels ((%skip-wildcard (str-lst)
             (skip-el wildcard str-lst :test #'char=))
           (%compare (swl sl)
             (let ((swl-1st (first swl)))
               (cond ((and swl sl)
                      (if (char= swl-1st wildcard)
                          (let* ((swl-rest (%skip-wildcard (rest swl)))
                                 (swl-2nd (first swl-rest)))                            
                            (if swl-2nd
                                (do ((sl-rest sl (rest sl-rest)))
                                    ((null sl-rest))
                                  (when (char= (first sl-rest) swl-2nd)
                                    (return (%compare (rest swl-rest) (rest sl-rest)))))
                                t))
                          (when (char= swl-1st (first sl))
                            (%compare (rest swl) (rest sl)))))
                     ((and swl (null sl))
                      (when (and (char= swl-1st wildcard)
                                 (not (first (%skip-wildcard (rest swl)))))
                        t))
                     ((and (null swl) sl) nil)
                     ((and (null swl) (null sl)) t)))))
    (if (and string-with-wildcards string)
        (let ((swl (coerce string-with-wildcards 'list))
              (sl (coerce string 'list)))
          (%compare swl sl))
        (error "STRING-EQUAL-WITH-WILDCARDS NEEDS ARGUMENTS THAT ARE NON-NIL"))))

;; http://stackoverflow.com/questions/11067899/is-there-a-generic-method-for-cloning-clos-objects
(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a shallow copy of OBJECT.
  An uninitialized object of the same class as OBJECT is allocated by
  calling ALLOCATE-INSTANCE.  For all slots returned by
  CLASS-SLOTS, the returned object has the
  same slot values and slot-unbound status as OBJECT.
  REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
           (copy (allocate-instance class)))
      (dolist (slot-name (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
            (slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))

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

(defmacro format-concatenate (stream &rest format-lists)
  "format-lists is a list of format lists in which first el is format-control
and rest is format-arguments.
example: (format-concatenate nil (\"~A ~A\" a b) (\"---\")"
  `(format ,stream "~A" (concatenate 'string
                                     ,@(mapcar (lambda (format-list) `(format nil ,@format-list)) format-lists))))

;; defmethods for aaaall objects

(defgeneric traverse-slots (obj fn)
  (:documentation "Use fn function on each slot. 'fn' must take two parameters: slot-name and slot-value."))

(defmethod traverse-slots (obj fn)
  (labels ((%traverse-slots (slots-lst)
             (when slots-lst
               (let* ((slot (car slots-lst))
                      (def-name (sb-mop:slot-definition-name slot))
                      (name (symbol-name def-name))
                      (value (slot-value obj def-name)))
                 (funcall fn name value)
                 (%traverse-slots (cdr slots-lst))))))
    (%traverse-slots (sb-mop:class-slots (class-of obj)))))

(defgeneric has-such-slot? (obj name value test)
  (:documentation "Checkes if object has a slot of a certain value"))

(defmethod has-such-slot? (obj name value test)
  (funcall test value (slot-value obj (intern (symbol-name name)))))

(defgeneric has-such-slot?/with-wildcard-string (obj name value wildcard)
  (:documentation "obj's slot by name 'name' is converted to string and compared to 'value'.
'value' can have a wildcard 'wildcard'.
If slot value is already a string it is not converted."))

(defmethod has-such-slot?/with-wildcard-string (obj name value wildcard)
  (let ((val (slot-value obj (intern (symbol-name name) (symbol-package (type-of obj))))))
    (string-equal-with-wildcards :string-with-wildcards value :string (if (typep val 'string) val (write-to-string val)) :wildcard wildcard)))

(defgeneric pretty-print-object (obj stream)
  (:documentation "Pretty printer for objects. Prints all slots with format 'SLOT-NAME: SLOT-VALUE'"))

(defmethod pretty-print-object (obj stream)
  (traverse-slots obj (lambda (name val) (format stream "~A: ~S~%" name val))))

