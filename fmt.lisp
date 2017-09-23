(in-package :df-cl-utils)

(defmacro fmt (stream fmt-string &rest fmt-args)
  ": instead of ~ and vice versa"
  `(format ,stream ,(%translate-fmt-string fmt-string) ,@fmt-args))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun %translate-fmt-string (fmt-string)
    (concatenate 'string
                 (loop for ch across fmt-string
                       collecting (cond ((char-equal ch #\~) #\:)
                                        ((char-equal ch #\:) #\~)
                                        (t ch)))))

  (defun %s-aux (stream format-letter format-lists fn)
    `(format ,stream
             ,(format nil "~~~A" (symbol-name format-letter))
             (concatenate 'string
                          ,@(mapcar
                             (lambda (format-list)
                               `(format nil ,@(cons (funcall fn (car format-list)) (rest format-list))))
                             format-lists))))

  (defun %l-aux (stream format-string-for-arg format-args fn)
    (let ((args-num (length format-args))
          (format-string-for-arg (funcall fn format-string-for-arg)))
      `(format ,stream ,(with-output-to-string (s) (loop repeat args-num collect (princ format-string-for-arg s)))
               ,@format-args))))

(defmacro fmt-l (stream fmt-string-for-arg &rest fmt-args)
  (%l-aux stream fmt-string-for-arg fmt-args #'%translate-fmt-string))

(defmacro fmts ((stream &optional (fmt-letter :A)) &rest fmt-lists)
  (%s-aux stream fmt-letter fmt-lists #'%translate-fmt-string))

(defmacro format-l (stream format-string-for-arg &rest format-args)
  (%l-aux stream format-string-for-arg format-args #'identity))

(defmacro formats ((stream &optional (format-letter :A)) &rest format-lists)
  (%s-aux stream format-letter format-lists #'identity))



