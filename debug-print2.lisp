(in-package #:df-cl-utils)

;; utils


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


(defgeneric pretty-print-object (obj stream)
  (:documentation "Pretty printer for objects. Prints all slots with format 'SLOT-NAME: SLOT-VALUE'"))

(defmethod pretty-print-object (obj stream)
  (format stream (with-output-to-string (s)
                   (traverse-slots obj (lambda (name val) (format s "~A: ~S~%" name val))))))

;;

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

(defmacro format* (fmt-or-string size &key (format-letter :A) align truncate? (stream nil))
  `(format*-aux ,(if (and (listp fmt-or-string) (stringp (first fmt-or-string))) `(format nil ,@fmt-or-string) fmt-or-string) ,size :format-letter ,format-letter :align ,align :truncate? ,truncate? :stream ,stream))

(defun format*-aux (string size &key format-letter align truncate? stream)
  (labels ((%wut-align ()
             (cond ((or (eq align :right) (eq align :r)) t)
                   ((or (eq align :left) (eq align :l)) nil)
                   (t nil)))
           (%truncate-string-maybe ()
             (if (and truncate? (> (length string) size)) (subseq string 0 size) string)))
    (format stream
            (format nil "~~~A~:[~;@~]~A" size (%wut-align) format-letter)
            (%truncate-string-maybe))))

;; vars

(defvar *dbp-counter* 0)
(defvar *dbp-format* nil)
(defvar *dbp-options* nil)

;;

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
  (oneline "-")
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
  (msg (make-instance 'fmt-msg)))

;; (build-fmt
;;  (:clip :size 1 :up "┌" :down "└" :vertical "│" :horizontal "─" :side :left)
;;  (:counter :size 4 :brace-left "" :brace-right ">")
;;  (:prefix :size 16 :brace-left "" :brace-right "")
;;  (:msg :size 60 :brace-left "" :brace-right ""))
(defun build-fmt (&rest clauses)
  (labels ((%parse-fmt-clauses (clauses)
             (mappend (lambda (clause)
                       (let ((type (first clause))
                             (initargs (rest clause)))
                         (list type `(make-instance ',(intern (format nil "FMT-~A" (symbol-name type)) :df-cl-utils) ,@initargs))))
                      clauses)))
    (if (equalp clauses'(nil))
        (make-instance 'fmt)
        (apply #'make-instance 'fmt (%parse-fmt-clauses clauses)))))

;; options

(defclass* options ()
  (out t) ; output stream
  (w-delim " ") ; thing that would delimit tokens
  (nl-command :nl)
  (nl-before-line-delim t) ; newline before line delimiter
  (nl-after-line-delim t) ; newline after line delimiter
  (use-prefix? t)
  (delim-command :d[?]) ; ? is where the delimiter 
  (delim-width 60)
  (literal-comand :l)
  (macros nil)) ; (:-> "arrow, lol" :<> "туди-сюди")

(defun build-options (&rest clauses)
  (if (equalp clauses '(nil))
      (make-instance 'options)
      (apply #'make-instance 'options clauses)))

;; prepare

(defmethod prepare-clip ((clip fmt-clip) place)
  (with-slots (up oneline down vertical horizontal size side brace-left brace-right) clip
    (labels ((%build-corner (corner-char)
               (let* ((corner-char (format* corner-char 1 :truncate? t))
                      (rest (format* horizontal (1- size) :truncate? t))
                      (left-part (if (eq side :left) corner-char rest))
                      (right-part (if (eq side :left) rest corner-char)))
                 (format nil "~A~A" left-part right-part)))
             (%build-mid (char)
               (format* char size :align side :truncate? t)))
      (case place
        (:upper (%build-corner up))
        (:oneline (%build-mid oneline))
        (:middle (%build-mid vertical))
        (:lower (%build-corner down))))))

(defmethod prepare-counter ((counter fmt-counter))
  (with-slots (size brace-left brace-right) counter
    (format* (apply-braces counter *dbp-counter*) size)))

(defmethod prepare-prefix ((prefix fmt-prefix) str)
  (with-slots (size brace-left brace-right) prefix
    (format* (apply-braces prefix str) size :truncate? t)))

;; (defmethod prepare-msg (()))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun form-gensym-name? (name)
    (and (> (length name) 13) (string-equal (subseq name 0 13) "_FORM_GENSYM_")))

  (defun parse-dbp-clauses (clauses)
    (let ((prefix-list nil)
          (msg-list nil)
          (id->form (make-hash-table))
          (context :msg)
          (fmt-init-list nil)
          (opts-init-list nil))
      (labels ((%context-dep-push (el)
                 (case context
                   (:msg (push el msg-list))
                   (:p   (push el prefix-list))
                   (t (error "wut"))))
               (%lst (lst)
                 (let ((el1 (first lst))
                       (id (gensym "_FORM_GENSYM_")))
                   (if (stringp el1)
                       (setf (gethash id id->form) `(format nil ,el1 ,@(rest lst)))
                       (setf (gethash id id->form) lst))
                   (%context-dep-push id)))
               (%lit () (%context-dep-push (pop clauses)))
               (%fmt () (setf fmt-init-list (pop clauses)))
               (%opts () (setf opts-init-list (pop clauses)))
               (%sym (sym)
                 (let ((name (symbol-name sym)))
                   (cond ((string-equal name "p") (setf context :p))
                         ((string-equal name "msg") (setf context :msg))
                         ((string-equal name "fmt") (%fmt))
                         ((string-equal name "opts") (%opts))
                         ((string-equal name "l") (%lit))
                         ((form-gensym-name? name) (%context-dep-push "_FORM_GENSYM_"))
                         (t (%context-dep-push sym)))))
               (%parse ()
                 (let ((el1 (pop clauses)))
                   (cond
                     ((stringp el1) (%context-dep-push el1))
                     ((listp el1)   (%lst el1))
                     ((symbolp el1) (%sym el1))
                     ((numberp el1) (%context-dep-push el1))))
                 (when clauses (%parse))))
        (%parse)
        (values prefix-list
                msg-list
                id->form
                (build-fmt fmt-init-list)
                (build-options opts-init-list))))))


(defun parser-test ()
  (multiple-value-bind (prefix-list msg-list forms frmt opts)
      (parse-dbp-clauses '(:p 1 2 3 4 msg opa opana :p heh  :l l msg l l her l p l msg :_f :_f ("~Afor~A" 'hop :real) l '("~s" 'anu) (format nil "hehmda") (loop for a from 0 to 10 do (prin1 a))))
    (format t "~s~%~s~%~s~%~s~%~s~%-----------------------------~%" prefix-list msg-list forms frmt opts)
    (traverse-slots frmt (lambda (name val) (format t "  ~S:~%" name) (pretty-print-object val t) (format t "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~%")))
    (format t "---------------------~%")
    (pretty-print-object opts t)))

(defmacro dbp2 (&body clauses)
  (multiple-value-bind (prefix-list msg-list id->form frmt opts)
      (parse-dbp-clauses clauses)
    (declare (ignore frmt))
    (let ((prefix-format-str (make-string-output-stream))
          (prefix-format-args nil)
          (msg-format-str (make-string-output-stream))
          (msg-format-args nil)
          (context nil))
      (labels ((%write-by-context (str)
                 (cond ((eq context :p) (format prefix-format-str "~A " str)) ((eq context :msg) (format msg-format-str "~A " str)) (t (error "wut2"))))
               (%add-arg-by-context (el)
                 (cond ((eq context :p) (push el prefix-format-args)) ((eq context :msg) (push el msg-format-args)) (t (error "wut3"))))
               (%write/add-arg (str el) (%write-by-context str) (%add-arg-by-context el))
               (%sym (sym)
                 (let ((wut-to-write (if (form-gensym-name? (symbol-name sym)) (gethash sym id->form) sym)))
                   (%write/add-arg "~A" wut-to-write)))
               (%process-element (el)
                 (cond ((stringp el) (%write-by-context el))
                       ((symbolp el) (%sym el))
                       ((listp el)   (%write/add-arg "~A" el))
                       ((numberp el) (%write-by-context el))))
               (%get-prefix-format-call ()
                 (setf context :p)
                 (dolist (i prefix-list) (%process-element i))
                 `(format ,(out opts) ,(get-output-stream-string prefix-format-str) ,@prefix-format-args))
               (%get-msg-format-call ()
                 (setf context :msg)
                 (dolist (i msg-list) (%process-element i))
                 `(format ,(out opts) ,(get-output-stream-string msg-format-str) ,@msg-format-args)))
        `(progn ,(%get-prefix-format-call)
                ,(%get-msg-format-call))))))

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
