(define-module (pnm core error)
  #:export (pnm-error))



(define (pnm-error message . args)
  (apply throw 'pnm-error message args))

;; error.scm ends here.
