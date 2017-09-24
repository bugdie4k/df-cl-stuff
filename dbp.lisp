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

  (defparameter *syntax* (make-hash-table))
  (defparameter *settings* (make-hash-table))

  (defmacro defcommands (class &rest clauses)
    `(let ((class-ht (make-hash-table)))
       (dolist (clause ',clauses)
         (destructuring-bind (name command) clause
           (setf (gethash name class-ht) command)))
       (setf (gethash ',class *syntax*) class-ht)))

  (defmacro get-com (class name)
    `(gethash ,name (gethash ',(intern (symbol-name class)) *syntax*)))

  (defcommands com1
    (:pref "p>")
    (:msg  "m>"))

  (defcommands com2
    (:lit "l")
    (:nl  "nl")
    (:cnl "cnl")
    (:delim "d"))

  (defmacro defsettings (&rest clauses)
    `(dolist (clause ',clauses)
       (destructuring-bind (name command default-value) clause
         (setf (gethash name *settings*) (cons command default-value)))))

  (defmacro get-setcom (name)
    `(car (gethash ,name *settings*)))

  (defmacro get-setdefault (name)
    `(cdr (gethash ,name *settings*)))

  (defsettings
    (:fmt "?fmt" nil)
    (:delim-len "?dl" 60)
    (:delim-nl-b? "?d-nl-b" t)
    (:delim-nl-a? "?d-nl-a" t)
    (:stream "?s" t)
    (:words-delim "?wd" " ")
    (:reset-counter "?rsc" nil)
    (:return "?return" nil))

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
                       (%context-dep-push `(fmt nil ,el1 ,@(rest lst)))
                       (%context-dep-push lst))))
               (%fmt () (setf fmt-init-list (pop clauses)))
               (%set-next (type) (%set type (pop clauses)))
               (%set (type val)
                 (push (cons val t) settings-list)
                 (push type settings-list))
               (%sym (sym)
                 (let ((name (symbol-name sym)))
                   (cond ((string-equal name (get-com :com1 :pref)) (setf context :p))
                         ((string-equal name (get-com :com1 :msg)) (setf context :msg))
                         ;; settings
                         ((string-equal name (get-setcom :fmt)) (%fmt))
                         ((string-equal name (get-setcom :words-delim)) (%set-next :words-delim))
                         ((string-equal name (get-setcom :delim-len)) (%set-next :delim-len))
                         ((string-equal name (get-setcom :delim-nl-b?)) (%set-next :delim-nl-b?))
                         ((string-equal name (get-setcom :delim-nl-a?)) (%set-next :delim-nl-a?))
                         ((string-equal name (get-setcom :stream)) (%set-next :stream))
                         ((string-equal name (get-setcom :reset-counter)) (%set :reset-counter t))
                         ((string-equal name (get-setcom :return)) (%set-next :return))
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
                settings-list)))))

(defmacro dbp (&body clauses)
  (multiple-value-bind (prefix-list msg-list frmt-list settings-list)
      (parse-dbp-clauses clauses)
    (fmt t ":A:%:A:%:A:%:A:%" prefix-list msg-list frmt-list settings-list)
    (let ((prefix-format-str (make-string-output-stream))
          (prefix-format-args nil)
          (msg-format-str (make-string-output-stream))
          (msg-format-args nil)
          (context nil)
          (lit? nil))
      (labels ((%getsetting (key)
                 (let ((res (getf settings-list key)))
                   (if (and (consp res)
                            (eq (cdr res) t))
                       (car res)
                       (get-setdefault key))))
               (%nl-or-delim? (sym)
                 (or (%nl? sym)
                     (and (symbolp sym)
                          (%delim? (symbol-name sym)))))
               (%nl? (sym)
                 (and (symbolp sym)
                      (string-equal (symbol-name sym) (get-com :com2 :nl))))
               (%write-by-context (str &optional (use-w-delim? t))
                 (let ((format-args `("~A~A" ,str ,(if (and use-w-delim?
                                                            (not (%nl-or-delim? use-w-delim?)))
                                                       (%getsetting :words-delim)
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
                 (let* ((len (%getsetting :delim-len))
                        (nl-bef (if (and (not (eq (%getsetting :delim-nl-b?) :no)) prev (not (%nl-or-delim? prev))) "~%" (if (and prev (not (%nl-or-delim? prev))) (%getsetting :words-delim) "")))
                        (nl-after (if (and (not (eq (%getsetting :delim-nl-a?) :no)) next (not (%nl? next))) "~%" (if (and next (not (%nl-or-delim? next))) (%getsetting :words-delim) ""))))
                   (format nil "~A~A~A"
                           nl-bef
                           (format+ ("~A" (with-output-to-string (s)
                                            (loop for i from 0 to (1+ (/ len (length delim-el))) do
                                              (format s "~A" delim-el))))
                                    len :truncate? t)
                           nl-after)))
               (%delim? (name)
                 (let* ((d (get-com :com2 :delim))
                        (dlen (length d)))
                   (and (> (length name) dlen)
                        (string-equal (subseq name 0 dlen) d))))
               (%delim (prev name next)
                 (%write-by-context (%construct-delim (subseq name (length (get-com :com2 :delim))) prev next) nil))
               (%sym (prev sym next)
                 (let ((name (symbol-name sym)))
                   (cond ((string-equal name (get-com :com2 :nl)) (%write-by-context "~%" nil))
                         ((string-equal name (get-com :com2 :cnl)) (%write-by-context "~&" nil))
                         ((%delim? name) (%delim prev name next))
                         ((string-equal name (get-com :com2 :lit)) (setf lit? t))
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
        `(progn ,(when (%getsetting :reset-counter) (dbp-reset))
                (print-message (build-fmt ,frmt-list)
                               :prefix-str ,(%get-prefix-format-call)
                               :msg-str    ,(%get-msg-format-call)
                               :return     ,(%getsetting :return)
                               :stream     ,(%getsetting :stream)))))))

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
        return))))

;; (df:dbp :m> d__________________________________x d_________________________________x d________________________________x d_______________________________x d______________________________x d_____________________________x d____________________________x d___________________________x d__________________________x d_________________________x d________________________x d_______________________x d______________________x d_____________________x d____________________x d___________________x d__________________x d_________________x d________________x d_______________x d______________x d_____________x d____________x d___________x d__________x d_________x d________x d_______x d______x d_____x d____x d___x d__x d_x dx d0 d8 ?rsc ?return 123 ?rsc)
