(in-package #:df-cl-utils)

;; utils

(defmacro defclass* (name direct-superclasses &rest direct-slots)
  `(defclass ,name ,direct-superclasses
     ,(when direct-slots
        (mapcar (lambda (slot-def)
                  (let ((slotname (first slot-def)))
                    (list slotname :accessor (intern (symbol-name slotname))
                                   :initarg (intern (symbol-name slotname) "KEYWORD")
                                   :initform (second slot-def))))
                direct-slots))))

(defun mappend (function &rest lists)
  "Applies FUNCTION to respective element(s) of each LIST, appending all the
all the result list to a single list. FUNCTION must return a list."
  (loop for results in (apply #'mapcar function lists)
        append results))

(defmacro format* (fmt-or-string size &key (format-letter :A) align truncate?)
  `(format*-aux ,(if (and (listp fmt-or-string) (stringp (first fmt-or-string))) `(format nil ,@fmt-or-string) fmt-or-string) ,size :format-letter ,format-letter :align ,align :truncate? ,truncate?))

(defun format*-aux (string size &key format-letter align truncate?)
  (labels ((%wut-align ()
             (cond ((or (eq align :right) (eq align :r)) t)
                   ((or (eq align :left) (eq align :l)) nil)
                   (t nil)))
           (%truncate-string-maybe ()
             (if (and truncate? (> (length string) size)) (subseq string 0 size) string)))
    (format nil
            (format nil "~~~A~:[~;@~]~A" size (%wut-align) format-letter)
            (%truncate-string-maybe))))

;; counter

(defvar *dbp-counter* 0)
(defvar *dbp-format* nil)
(defvar *dbp-options* nil)
