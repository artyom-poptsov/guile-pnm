(define-module (pnm core common)
  #:use-module (oop goops)
  #:export (constructor-argument
            object-address/hex-string))


(define (object-address/hex-string object)
  (number->string (object-address object) 16))


(define (constructor-argument keyword initargs)
  (and (memq keyword initargs)
       (cadr (memq keyword initargs))))

;; common.scm ends here.
