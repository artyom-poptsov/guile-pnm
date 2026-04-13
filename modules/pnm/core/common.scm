(define-module (pnm core common)
  #:use-module (oop goops)
  #:export (constructor-argument))

(define (constructor-argument keyword initargs)
  (and (memq keyword initargs)
       (cadr (memq keyword initargs))))

;; common.scm ends here.
