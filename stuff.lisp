(in-package #:df-cl-utils)

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

(defmacro defclass+ (name direct-superclasses &rest direct-slots)
  "Simpler class definition.
Defining slots you may specify initform as the second el,
and type as third."
  (labels ((%get-slot-def (sym &optional (initform nil))
             (list sym :accessor (intern (symbol-name sym))
                       :initarg (intern (symbol-name sym) "KEYWORD")
                       :initform initform)))
    `(defclass ,name ,direct-superclasses
       ,(when direct-slots
          (mapcar (lambda (slot-def)
                    (if (symbolp slot-def)
                        (%get-slot-def slot-def)
                        (let* ((slotname (first slot-def))
                               (res-slot-def
                                (%get-slot-def slotname (second slot-def)))
                               (type? (third slot-def)))
                          (if type? (append res-slot-def `(:type ,type?))
                              res-slot-def))))
                  direct-slots)))))

(defmacro mv-let* (mv-let-list &body body)
  "(mv-let* (((v v v) form)
             ((v v) form))
      body)"
  (labels ((%expand-mv-let* (mv-let-list)
             (let ((mv-let1 (pop mv-let-list)))
               (if mv-let-list
                   `(multiple-value-bind ,(first mv-let1) ,(second mv-let1)
                      ,(%expand-mv-let* mv-let-list))
                   `(multiple-value-bind ,(first mv-let1) ,(second mv-let1)
                     ,@body)))))
    (%expand-mv-let* mv-let-list)))

(defun mappend (function &rest lists)
  (loop for results in (apply #'mapcar function lists)
        append results))

;; traverse object slots

(defgeneric traverse-slots (obj fn)
  (:documentation
   "Use fn function on each slot. 
'fn' must take two parameters: slot-name and slot-value."))

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

(defgeneric has-slot? (obj name value test)
  (:documentation "Checkes if object has a slot of a certain value"))

(defmethod has-slot? (obj name value test)
  (funcall test value (slot-value obj (intern (symbol-name name)))))

(defgeneric pprint-object (obj stream)
  (:documentation
   "Pretty printer for objects.
Prints all slots with format 'SLOT-NAME: SLOT-VALUE'"))

(defmethod pprint-object (obj stream)
  (format stream
          (with-output-to-string (s)
            (traverse-slots
             obj
             (lambda (name val)
               (format s "~A: ~S~%" name val))))))

;;

(defun clean-string (string)
  (let ((spaces? nil)
        (newlines? nil))
    (string-trim (format nil " ~%")
                 (coerce (loop for c across string
                               unless (or (and spaces? (char= c #\space))
                                          (and newlines? (char= c #\newline)))
                                 collect c
                               do (if (char= c #\space) (setf spaces? t) (setf spaces? nil))
                               (if (char= c #\newline) (setf newlines? t) (setf newlines? nil)))
                         'string))))

(defun to-printable-string (arg)
  (clean-string (write-to-string arg)))

(defun mkup (thing)
  (labels ((%sharp-replace (str)
             (coerce (loop for c across str
                           collect
                           (cond ((char= c #\#) #\space)
                                 ((or (char= c #\<)
                                      (char= c #\>)) #\space)
                                 (t c)))
                     'string)))
    (write-to-string (read-from-string (%sharp-replace (to-printable-string thing))))))

(defun string+ (&rest strings)
  (format nil "~{~A~}" strings))

