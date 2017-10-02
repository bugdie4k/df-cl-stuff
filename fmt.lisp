(in-package :df-cl-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun %parse-fmt-args (args)
    (let* ((default-stream? (stringp (first args)))
           (stream (if default-stream? t (first args)))
           (fmt-string (if default-stream? (first args) (second args)))
           (fmt-args (if default-stream? (cdr args) (cddr args))))
      (values stream fmt-string fmt-args)))

  (defun %parse-fmts-args (args)
    (let* ((default-stream? (listp (first args)))
           (stream (if default-stream? t (first args)))
           (fmt-lists (if default-stream? args (cdr args))))
      (values stream fmt-lists)))

  (defun %translate-fmt-string (fmt-string)
    (concatenate 'string
                 (loop for ch across fmt-string
                       collecting (cond ((char-equal ch #\~) #\:)
                                        ((char-equal ch #\:) #\~)
                                        (t ch)))))

  (defun %s-aux (stream format-lists fn)
    `(progn
       ,@(mapcar
          (lambda (format-list)
            `(format ,stream ,@(cons (funcall fn (car format-list)) (rest format-list))))
          format-lists)))

  (defun %4l-aux (stream format-string-for-arg format-args fn)
    (let ((args-num (length format-args))
          (format-string-for-arg (funcall fn format-string-for-arg)))
      `(format ,stream ,(with-output-to-string (s) (loop repeat args-num collect (princ format-string-for-arg s)))
               ,@format-args))))

(defmacro fmt (&rest args)
  ": instead of ~ and vice versa
you can ommit stream in call, t is default"
  (multiple-value-bind (stream fmt-string fmt-args) (%parse-fmt-args args)
    `(format ,stream ,(%translate-fmt-string fmt-string) ,@fmt-args)))

(defmacro fmt4l (&rest args)
  "fmt for list,
you can ommit stream in call, t is default"
  (multiple-value-bind (stream fmt-string-for-arg fmt-args) (%parse-fmt-args args)
    (%4l-aux stream fmt-string-for-arg fmt-args #'%translate-fmt-string)))

(defmacro fmts (&rest args)
  "(fmts stream (\":A:A\" a b) (\":20A:20A\" c d))
you can ommit stream in call, t is default"
  (multiple-value-bind (stream fmt-lists) (%parse-fmts-args args)
    (%s-aux stream fmt-lists #'%translate-fmt-string)))

(defmacro format4l (&rest args)
  (multiple-value-bind (stream format-string-for-arg format-args) (%parse-fmt-args args)
    (%4l-aux stream format-string-for-arg format-args #'identity)))

(defmacro formats (&rest args)
  (multiple-value-bind (stream format-lists) (%parse-fmts-args args)
    (%s-aux stream format-lists #'identity)))



