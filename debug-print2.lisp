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

(defun count++ ()
  (incf *dbp-counter*))

(defun dbp-reset-counter ()
  (setf *dbp-counter* 0))

;; format

(defclass* fmt-size-mixin ()
  (size 1))

(defclass* fmt-braces-mixin ()
  (brace-left "")
  (brace-right ""))

(defmethod apply-braces ((braces fmt-braces-mixin) str &key (format-string "~A~A~A"))
  (with-slots (brace-left brace-right) braces
    (format nil format-string brace-left str brace-right)))

(defclass* fmt-clip (fmt-size-mixin)
  (size 1)
  (up "┌")
  (down "└")
  (horizontal "─")
  (vertical "│")
  (side :left))

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
  (order '(:clip :counter :msg)))

;; EXAMPLE:

;; (build-fmt
;;  (:clip :size 1 :up "┌" :down "└" :vertical "│" :horizontal "─" :side :left)
;;  (:counter :size 4 :brace-left "" :brace-right ">")
;;  (:prefix :size 16 :brace-left "" :brace-right "")
;;  (:msg :size 60 :brace-left "" :brace-right "")
;;  (:order :clip :counter :prefix :msg))
(defun build-fmt (&rest clauses)
  (labels ((%parse-fmt-clauses (clauses)
             (mappend (lambda (clause)
                       (let ((type (first clause))
                             (initargs (rest clause)))
                         (if (eq type :order)
                             `(:order ',initargs)
                             (apply #'make-instance (intern (format nil "FMT-~A" (symbol-name type)) :df-cl-utils) initargs))))
                      clauses)))
    (apply #'make-instance 'fmt (%parse-fmt-clauses clauses))))

;; options

(defclass* options ()
  (out t) ; output stream
  (w-delim " ") ; thing that would delimit tokens
  (nl-command :nl)
  (nl-before-line-delim t) ; newline before line delimiter
  (nl-after-line-delim t) ; newline after line delimiter
  (delim-command :d[?]) ; ? is where the delimiter 
  (delim-width 60)
  (macros nil)) ; (:-> "arrow, lol" :<> "туди-сюди")

(defun build-options (&rest clauses)
  (apply #'make-instance 'options clauses))

;; printing message

(defmethod prepare-clip ((clip fmt-clip) place)
  (with-slots (up down vertical horizontal size side brace-left brace-right) clip
    (labels ((%build-corner (corner-char)
               (let* ((corner-char (format* corner-char 1 :truncate? t))
                      (rest (format* horizontal (1- size) :truncate? t))
                      (left-part (if (eq side :left) corner-char rest))
                      (right-part (if (eq side :left) rest corner-char)))
                 (format nil "~A~A" left-part right-part))))
      (case place
        (:upper (%build-corner up))
        (:middle (format* vertical size :align side :truncate? t))
        (:lower (%build-corner down))))))

(defmethod prepare-counter ((counter fmt-counter))
  (with-slots (size brace-left brace-right) counter
    (format* (apply-braces counter *dbp-counter*) size)))

(defmethod prepare-prefix ((prefix fmt-prefix) str)
  (with-slots (size brace-left brace-right) prefix
    (format* (apply-braces prefix str) size :truncate? t)))

;; (defmethod print-message ((frmt fmt) (opts options) msg-lst)
;;   (with-slots (frmt-clip frmt-counter frmt-prefix frmt-msg frmt-order) frmt
;;     (with-slots (opts-out opts-w-delim opts-nl-before-line-delim opts-nl-after-line-delim opts-use-prefix? opts-macros) opts
;;       (labels ((%process-element (el)
;;                  )
;;                (%get-prefix ()
;;                  (let ((el0 (first msg-list)))
;;                    (%process-element el0))))
;;         (let* ((clip-upper-str (prepare-clip frmt-clip :upper))
;;                (clip-middle-str (prepare-clip frmt-clip :middle))
;;                (clip-lower-str (prepare-clip frmt-clip :lower))
;;                (counter-str (prepare-counter frmt-counter))
;;                (prefix-str (if opts-use-prefix? (prepare-prefix frmt-prefix (%get-prefix)) ""))))))))

;; (defun dbp-extended (&key (msg-lst nil) (opts nil) (frmt nil))
;;   (let ((opts (build-options opts))
;;         (frmt (build-fmt fmt)))
;;     (print-message frmt opts msg-lst)))

;; (defmacro dbp ())

