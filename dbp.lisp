(in-package #:df-cl-utils)

;; format+ 

(defmacro format+ (fmt-or-string size &key (format-letter :A) align truncate? (stream nil))
  `(format+-aux ,(if (and (listp fmt-or-string) (stringp (first fmt-or-string))) `(format nil ,@fmt-or-string) fmt-or-string) ,size :format-letter ,format-letter :align ,align :truncate? ,truncate? :stream ,stream))

(defun format+-aux (string size &key format-letter align truncate? stream)
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
;; (defvar *dbp-format* nil)

(defun dbp-reset ()
  (setf *dbp-counter* 0))

;; format

(defclasss fmt-size-mixin ()
  (size 1))

(defclasss fmt-braces-mixin ()
  (brace-left "")
  (brace-right ""))

(defmethod apply-braces ((braces fmt-braces-mixin) str &key (format-string "~A~A~A"))
  (with-slots (brace-left brace-right) braces
    (format nil format-string brace-left str brace-right)))

(defclasss fmt-clip (fmt-size-mixin)
  (size 2)
  (up "┌")
  (down "└")
  ;; (horizontal "─")
  (horizontal " ")
  (vertical "│")
  (oneline "•")
  (side :left))

(defclasss fmt-counter (fmt-size-mixin fmt-braces-mixin)
  (size 6)
  (brace-right "> "))

(defclasss fmt-prefix (fmt-size-mixin fmt-braces-mixin)
  (size 16))

(defclasss fmt-msg (fmt-size-mixin fmt-braces-mixin)
  (size 60))

(defclasss fmt ()
  (clip (make-instance 'fmt-clip))
  (counter (make-instance 'fmt-counter))
  (prefix (make-instance 'fmt-prefix))
  (msg (make-instance 'fmt-msg)))

(defun build-fmt (clauses)
  (labels ((%parse-fmt-clauses (clauses)
             (mappend (lambda (clause)
                       (let ((type (first clause))
                             (initargs (rest clause)))
                         (list type (apply #'make-instance (intern (format nil "FMT-~A" (symbol-name type)) :df-cl-utils) initargs))))
                      clauses)))
    (if (equalp clauses'(nil))
        (make-instance 'fmt)
        (apply #'make-instance 'fmt (%parse-fmt-clauses clauses)))
    ))

;; prepare

(defmethod prepare-clip ((clip fmt-clip) place)
  (with-slots (up oneline down vertical horizontal size side brace-left brace-right) clip
    (labels ((%build-corner (corner-char)
               (let* ((corner-char (format+ corner-char 1 :truncate? t))
                      (rest (format+ horizontal (1- size) :truncate? t))
                      (left-part (if (eq side :left) corner-char rest))
                      (right-part (if (eq side :left) rest corner-char)))
                 (format nil "~A~A" left-part right-part)))
             (%build-mid (char)
               (format+ char size :align side :truncate? t)))
      (case place
        (:upper (%build-corner up))
        (:oneline (%build-mid oneline))
        (:middle (%build-mid vertical))
        (:lower (%build-corner down))))))

(defmethod prepare-clips ((clip fmt-clip))
  (list (prepare-clip clip :upper)
        (prepare-clip clip :oneline)
        (prepare-clip clip :middle)
        (prepare-clip clip :lower)))

(defmethod prepare-counter ((counter fmt-counter))
  (with-slots (size brace-left brace-right) counter
     (format+ (apply-braces counter *dbp-counter*) size)))

(defmethod prepare-prefix ((prefix fmt-prefix) str)
  (if str
      (with-slots (size brace-left brace-right) prefix
        (format+ (apply-braces prefix str) size :truncate? t))
      ""))


(eval-when (:compile-toplevel :load-toplevel :execute)

  ;; syntax settings
  (defparameter *syntax-settings*
    (let ((ht (make-hash-table)))
      (loop for (k v) on '(;; constructing commands
                           :com1/pref "p>"
                           :com1/msg  "m>"
                           ;; string control commands
                           :com2/lit   "l"
                           :com2/nl    "nl"
                           :com2/cnl   "cnl"
                           :com2/delim "d"
                           :com2/string "str" ;; TODO <--
                           ;; settings
                           :set/fmt           "?fmt"
                           :set/words-delim   "?wd"
                           :set/delim-len     "?dl"
                           :set/delim-nl-b?   "?d-nl-b"
                           :set/delim-nl-a?   "?d-nl-a"
                           :set/stream        "?s"
                           :set/reset-counter "?rs" ;; TODO <--
                           )
            do (setf (gethash k ht) v))
      ht))

  (defparameter *syntax* (make-hash-table))
  (defparameter *settings* (make-hash-table))

  (defun defcommand-aux (class clauses)
    (let ((class-ht (make-hash-table)))
      (dolist (clause clauses)
        (destructuring-bind (name command) clause
          (setf (gethash name class-ht) command)))
      (setf (gethash class *syntax*) class-ht)))

  (defmacro defcommands (class &rest clauses)
    (defcommand-aux class clauses))

  (defmacro getcom (class name)
    `(gethash ',name (gethash ',class *syntax*)))

  (defcommands com1
    (pref "p>")
    (msg  "m>"))

  (defcommands com2
    (lit "l")
    (nl  "nl")
    (cnl "cnl")
    (delim "d"))

  (defun defsettings-aux (clauses)
    (dolist (clause clauses)
      (destructuring-bind (name command default-state) clause
        (setf (gethash name *settings*) (cons command default-state)))))

  (defmacro defsettings (&rest clauses)
    (defsettings-aux clauses))

  (defmacro getsetcom (name)
    `(car (gethash ',name *settings*)))

  (defmacro getsetval (name)
    `(cdr (gethash ',name *settings*)))

  (defmacro setsetval (name val)
    (setf (gethash ',name *settings*)
          (cons ))) ;; TODO <--

  (defun getcom1 (key) (gethash (intern (concatenate 'string "COM1/" (symbol-name key)) :KEYWORD) *syntax-settings*))
  (defun getcom2 (key) (gethash (intern (concatenate 'string "COM2/" (symbol-name key)) :KEYWORD) *syntax-settings*))
  (defun getset  (key) (gethash (intern (concatenate 'string "SET/" (symbol-name key)) :KEYWORD) *syntax-settings*))

  (defparameter *default-setting-values* '(:fmt nil :words-delim " " :delim-len 60 :delim-nl-b? t :delim-nl-a? t :stream t))

  (defun getdefset (key) (getf *default-setting-values* key))

  (defun parse-dbp-clauses (clauses)
    (let ((prefix-list nil)
          (msg-list nil)
          (context :msg)
          (fmt-init-list nil)
          (settings-list nil))
      (labels ((%context-dep-push (el)
                 (case context
                   (:msg (push el msg-list))
                   (:p   (push el prefix-list))
                   (t (error "wut"))))
               (%lst (lst)
                 (let ((el1 (first lst)))
                   (if (stringp el1)
                       (%context-dep-push `(format nil ,el1 ,@(rest lst)))
                       (%context-dep-push lst))))
               (%fmt () (setf fmt-init-list (pop clauses)))
               (%set (type) (push (pop clauses) settings-list) (push type settings-list))
               (%sym (sym)
                 (let ((name (symbol-name sym)))
                   (cond ((string-equal name (getcom1 :pref)) (setf context :p))
                         ((string-equal name (getcom1 :msg)) (setf context :msg))
                         ((string-equal name (getset :fmt)) (%fmt))
                         ((string-equal name (getset :words-delim)) (%set :words-delim))
                         ((string-equal name (getset :delim-len)) (%set :delim-len))
                         ((string-equal name (getset :delim-nl-b?)) (%set :delim-nl-b?))
                         ((string-equal name (getset :delim-nl-a?)) (%set :delim-nl-a?))
                         ((string-equal name (getset :stream)) (%set :stream))
                         (t (%context-dep-push sym)))))
               (%parse ()
                 (let ((el1 (pop clauses)))
                   (cond
                     ((stringp el1) (%context-dep-push el1))
                     ((listp el1)   (%lst el1))
                     ((symbolp el1) (%sym el1))
                     ((numberp el1) (%context-dep-push el1))))
                 (when clauses (%parse))))
        (when clauses (%parse))
        (values (nreverse prefix-list)
                (nreverse msg-list)
                fmt-init-list
                settings-list))))

  (defmacro dbp (&body clauses)
    (multiple-value-bind (prefix-list msg-list frmt-list settings-list) ; opts
        (parse-dbp-clauses clauses)
      ;; (format t "~A - ~A - ~A - ~A~%" prefix-list msg-list frmt-list settings-list)
      (let ((prefix-format-str (make-string-output-stream))
            (prefix-format-args nil)
            (msg-format-str (make-string-output-stream))
            (msg-format-args nil)
            (context nil)
            (lit? nil))
        (labels ((%getset (key) (or (getf settings-list key)
                                    (getdefset key)))
                 (%nl? (el)
                   (and (symbolp el)
                        (or (string-equal (symbol-name el) (getcom2 :nl))
                            (%delim? (symbol-name el)))))
                 (%write-by-context (str &optional (use-w-delim? t))
                   (let ((format-args `("~A~A" ,str ,(if (and use-w-delim?
                                                              (not (%nl? use-w-delim?)))
                                                         (%getset :words-delim)
                                                         ""))))
                     (cond ((eq context :p) (apply #'format prefix-format-str format-args))
                           ((eq context :msg) (apply #'format msg-format-str format-args))
                           (t (error "wut2")))))
                 (%add-arg-by-context (el)
                   (cond ((eq context :p) (setf prefix-format-args (append prefix-format-args (list el))))
                         ((eq context :msg) (setf msg-format-args (append msg-format-args (list el))))
                         (t (error "wut3"))))
                 (%write/add-arg (str el &optional (use-w-delim? t))
                   (%write-by-context str use-w-delim?) (%add-arg-by-context el))
                 (%construct-delim (delim-el prev next)
                   (let* ((len (%getset :delim-len))
                          (nl-bef (if (and (not (eq (%getset :delim-nl-b?) :no)) prev (not (%nl? prev))) "~%" (if (and prev (not (%nl? prev))) (%getset :words-delim) "")))
                          (nl-after (if (and (not (eq (%getset :delim-nl-a?) :no)) next (not (%nl? next))) "~%" (if (and next (not (%nl? next))) (%getset :words-delim) ""))))
                     (format nil "~A~A~A"
                             nl-bef
                             (format+ ("~A" (with-output-to-string (s)
                                              (loop for i from 0 to (1+ (/ len (length delim-el))) do
                                                (format s "~A" delim-el))))
                                      len :truncate? t)
                             nl-after)))
                 (%delim? (name)
                   (let* ((d (getcom2 :delim))
                          (dlen (length d)))
                     (and (> (length name) dlen)
                          (string-equal (subseq name 0 dlen) d))))
                 (%delim (prev name next)
                   (%write-by-context (%construct-delim (subseq name (length (getcom2 :delim))) prev next) nil))
                 (%sym (prev sym next)
                   (let ((name (symbol-name sym)))
                     (cond ((string-equal name (getcom2 :nl)) (%write-by-context "~%" nil))
                           ;; ((string-equal name (getcom2 :cnl)) (%write-by-context "~_" nil))
                           ((%delim? name) (%delim prev name next))
                           ((string-equal name (getcom2 :lit)) (setf lit? t))
                           (t (%write/add-arg "~A" sym next)))))
                 (%process-element (prev el next)
                   (if lit?
                       (progn (%write/add-arg "~A" el next) (setf lit? nil))
                       (cond ((stringp el) (%write-by-context el next))
                             ((symbolp el) (%sym prev el next))
                             ((listp el)   (%write/add-arg "~A" el next))
                             ((numberp el) (%write-by-context el next)))))
                 (%get-prefix-format-call ()
                   (when prefix-list
                     (setf context :p)
                     (let (prev)
                       (loop for (a b) on prefix-list do (%process-element prev a b) (setf prev a))
                       `(format nil ,(get-output-stream-string prefix-format-str) ,@prefix-format-args))))
                 (%get-msg-format-call ()
                   (when msg-list
                     (setf context :msg)
                     (let (prev)
                       (loop for (a b) on msg-list do (%process-element prev a b) (setf prev a))
                       `(format nil ,(get-output-stream-string msg-format-str) ,@msg-format-args)))))
          `(print-message (build-fmt ,frmt-list)
                          :prefix-str ,(%get-prefix-format-call)
                          :msg-str    ,(%get-msg-format-call)
                          :return     nil ;; TODO <--
                          :stream     ,(%getset :stream)))))))

(defmethod print-message ((frmt fmt) &key prefix-str msg-str return stream)
  (destructuring-bind (up-clip oneline-clip mid-clip down-clip) (prepare-clips (clip frmt))
    (let ((prefix-str (prepare-prefix (prefix frmt) prefix-str))
          (counter-str (prog1 (prepare-counter (counter frmt)) (incf *dbp-counter*)))
          (msg-str (or msg-str "")))
      (labels ((%count-msg-lines ()
                 (loop for ch across msg-str count (char-equal ch #\newline)))
               (%clip-decide (ln-num nls)
                 (cond
                   ((= ln-num nls) down-clip)
                   (t mid-clip)))
               (%insert-prefixes ()
                 (let ((nls (%count-msg-lines)))
                   (if (= nls 0)
                       (format nil "~A~A~A~A" oneline-clip counter-str prefix-str msg-str)
                       (with-output-to-string (s)
                         (format s "~A~A~A" up-clip counter-str prefix-str)
                         (loop for ch across msg-str
                               with i = 0
                               do (format s "~c" ch)
                                  (when (char-equal ch #\newline)
                                    (incf i)
                                    (format s "~A~A~A" (%clip-decide i nls) counter-str prefix-str))))))))
        (format stream (%insert-prefixes))
        (format stream "~%")
        (apply #'values return)))))
