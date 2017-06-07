(defpackage #:df-cl-utils
  (:nicknames #:dfclu #:df)
  (:use #:cl)
  (:export #:skip-el
           #:string-equal-with-wildcards
           #:copy-instance
           #:replace-in-tree
           #:mv-let*
           #:format-concatenate
           #:traverse-slots
           #:has-such-slot?
           #:has-such-slot?/with-wildcard-string
           #:pretty-print-object
           ;; debug print
           #:dbp
           #:dbpe
           #:dbp-reset
           #:dbps
           #:make-up
           #:clean-string))
