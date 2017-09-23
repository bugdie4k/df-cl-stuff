(defmacro fmt ())

(defmacro format-concatenate (stream &rest format-lists)
  "format-lists is a list of format lists in which first el is format-control
and rest is format-arguments.
example: (format-concatenate nil (\"~A ~A\" a b) (\"---\")"
  `(format ,stream "~A" (concatenate 'string
                                     ,@(mapcar (lambda (format-list) `(format nil ,@format-list)) format-lists))))



