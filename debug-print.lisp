(in-package #:df-cl-utils)

(defvar *dbp-count* 0)

(defparameter *dbp-standard-args* '(:increase-count? t :stream t :place-for-count 4 :place-for-first-in-prefix 16 :trailing-newline? t :words-delimiter #\space :add-prefixes? t :use-first-as-prefix-part? t
                                    :line-delimiter-char #\-  :line-delimiter-char1 #\= :line-delimiter-char2 #\* :line-delimiter-char3 #\# :line-delimiter-char4 #\~ :line-delimiter-char5 #\> :line-delimiter-char6 #\<
                                    :line-delimiter-length 40 :line-delimiter-width 1
                                    :mark-sections? t))

(defun dbpe (&key (stream t) (increase-count? t) (place-for-count 4) (place-for-first-in-prefix 16) (trailing-newline? t)
               (words-delimiter #\space) (add-prefixes? t) (use-first-as-prefix-part? t) (line-delimiter-char #\-) (line-delimiter-char1 #\=) (line-delimiter-char2 #\*) (line-delimiter-char3 #\#) (line-delimiter-char4 #\~)
               (line-delimiter-char5 #\>) (line-delimiter-char6 #\<)
               (line-delimiter-length 20) (line-delimiter-width 1)
               (mark-sections? t)
               (output-list (error "DBPE NEEDS OUTPUT-LIST ARGUMENT")))
  "Extended. Use :nl and :delim args to make newline and delimit sections."
  (let* ((prefix-count (format nil
                               (format nil "~~~AA " place-for-count)
                               (format nil "~A>" *dbp-count*)))
         (prefix (if use-first-as-prefix-part?
                     (format nil
                             (format nil "~A ~~~AA " prefix-count place-for-first-in-prefix)
                             (format nil "~A" (car output-list)))
                     prefix-count))
         (output-list (if use-first-as-prefix-part? (cdr output-list) output-list))
         (marker-count 1))
    (labels ((%sym-delim? (sym)
               (when (symbolp sym)
                 (let ((sym-name (symbol-name sym)))
                   (cond
                     ((= (length sym-name) 6)
                      (let ((sym-name-0to5 (subseq sym-name 0 5))
                             (sym-name-5toend (subseq sym-name 5)))
                         (when (string= sym-name-0to5 "DELIM")
                           (find (parse-integer sym-name-5toend :junk-allowed t) '(1 2 3 4 5 6)))))
                     ((and (= (length sym-name) 5) (string= sym-name "DELIM")) 0)))))
             (%apply-autistic-formatting (list)
               (let (prev)
                 (loop for (el el2) on list
                       for i from 0
                       collecting (let ((delim? (%sym-delim? el)))
                                    (cond ((eq el :nl) (format nil "~%"))
                                          (delim? (format nil "~:[~%~;~]~{~A~^~%~}~:[~;~%~]"
                                                          (or (zerop i) (%sym-delim? prev))
                                                          (loop for i from 1 to line-delimiter-width                                                                 
                                                                collect (make-string line-delimiter-length :initial-element (ecase delim?
                                                                                                                              (0 line-delimiter-char)
                                                                                                                              (1 line-delimiter-char1)
                                                                                                                              (2 line-delimiter-char2)
                                                                                                                              (3 line-delimiter-char3)
                                                                                                                              (4 line-delimiter-char4)
                                                                                                                              (5 line-delimiter-char5)
                                                                                                                              (6 line-delimiter-char6))))
                                                          el2))
                                          (t (format nil "~A~A" el words-delimiter))))
                       do (setf prev el))))
             (%intercept-newlines-with-prefix (string)
               (let ((newlines (loop for c across string counting (eq c #\newline))))
                 (coerce
                  (loop for c across string
                        appending (if (eq c #\newline)
                                      (let ((prefix-chars (coerce prefix 'list)))
                                        (cons #\newline (if mark-sections?
                                                            (append (if (= marker-count newlines)
                                                                        (coerce "*** " 'list)
                                                                        (progn (incf marker-count) (coerce "*   " 'list)))
                                                                    prefix-chars)
                                                            prefix-chars)))
                                      (list c)))
                  'string))))
      (let ((str (format nil "~@[~{~A~}~]" (%apply-autistic-formatting output-list))))
        (format stream "~A~:[~;~%~]" (format nil "~:[~;*-> ~]~A~A" mark-sections? prefix (if add-prefixes? (%intercept-newlines-with-prefix str) str)) trailing-newline?)
        (when increase-count? (incf *dbp-count*))))))

(defun dbp (&rest output-list)
  "Simple"
  (apply #'dbpe :output-list output-list *dbp-standard-args*))

(defmacro dbps (&rest output-list-list)
  "Several with implicit :nl"
  (let ((1st (car output-list-list))
        (output-list-list (cdr output-list-list)))
    `(progn 
       ,@(mapcar (lambda (output-list)
                   `(apply #'dbpe :output-list ',(cons 1st output-list) :mark-sections? nil :increase-count? nil',*dbp-standard-args*))
                 (butlast output-list-list))
       (apply #'dbpe :output-list ',(cons 1st (car (last output-list-list))) :mark-sections? nil ',*dbp-standard-args*))))

(defun dbp-reset ()
  (setf *dbp-count* 0))
