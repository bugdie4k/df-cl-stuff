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

;; format

(defclass* fmt-size-mixin ()
  (size 1))

(defclass* fmt-clip (fmt-size-mixin)
  (up "┌")
  (down "└")
  (horizontal "─")
  (vertical "│"))

(defclass* fmt-braces-mixin ()
  (brace-left "")
  (brace-right ""))

(defclass* fmt-counter (fmt-size-mixin fmt-braces-mixin)
  (brace-right ">"))

(defclass* fmt-prefix (fmt-size-mixin fmt-braces-mixin))

(defclass* fmt-msg (fmt-size-mixin))

(defclass* fmt ()
  (clip (make-instance 'fmt-clip :size 1))
  (counter (make-instance 'fmt-counter :size 4))
  (prefix (make-instance 'fmt-prefix :size 16))
  (msg (make-instance 'fmt-msg :size 60)))

;;
