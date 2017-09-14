(in-package #:df-cl-utils)

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

;; format

(defclass* fmt-size-mixin ()
  (size 1))

(defclass* fmt-clip (fmt-size-mixin)
  (size 1)
  (up "┌")
  (down "└")
  (horizontal "─")
  (vertical "│")
  (side :left))

(defclass* fmt-braces-mixin ()
  (brace-left "")
  (brace-right ""))

(defclass* fmt-counter (fmt-size-mixin fmt-braces-mixin)
  (size 4)
  (brace-right ">"))

(defclass* fmt-prefix (fmt-size-mixin fmt-braces-mixin)
  (size 16))

(defclass* fmt-msg (fmt-size-mixin fmt-braces-mixin)
  (size 60))

(defclass* fmt ()
  (clip (make-instance 'fmt-clip))
  (counter (make-instance 'fmt-counter))
  (prefix (make-instance 'fmt-prefix))
  (msg (make-instance 'fmt-msg))
  (order '(:clip :counter :prefix :msg)))

;; EXAMPLE:

;; (build-fmt
;;  (:clip :size 1 :up "┌" :down "└" :vertical "│" :horizontal "─" :side :left)
;;  (:counter :size 4 :brace-left "" :brace-right ">")
;;  (:prefix :size 16 :brace-left "" :brace-right "")
;;  (:msg :size 60 :brace-left "" :brace-right "")
;;  (:order :clip :counter :prefix :msg))
(defmacro build-fmt (&rest clauses)
  (labels ((%parse-fmt-clauses (clauses)
             (mappend (lambda (clause)
                       (let ((type (first clause))
                             (initargs (rest clause)))
                         (if (eq type :order)
                             `(:order ',initargs)
                             (list type `(make-instance ',(intern (format nil "FMT-~A" (symbol-name type)) :df-cl-utils) ,@initargs)))))
                     clauses)))
    `(make-instance 'fmt ,@(%parse-fmt-clauses clauses))))

;; options

(defmacro build-options (&rest clauses)
  ())

(defun dbp-extended (&key (msg (error "DBPE NEEDS OUTPUT-LIST ARGUMENT")) (fmt nil) (options nil))
  )

(defmacro dbp ())

