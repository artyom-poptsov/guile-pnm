(define-module (pnm fsm pnm-context)
  #:use-module (pnm core error)
  #:use-module (pnm fsm common)
  #:use-module (pnm fsm context)
  #:use-module (ice-9 textual-ports)
  #:re-export (throw-magic-number-error
               throw-unexpected-eof
               throw-format-error
               none)
  #:export (throw-unsupported-format
            set-type-pbm-ascii
            set-type-pgm-ascii
            set-type-ppm-ascii
            set-type-pbm-binary
            set-type-pgm-binary
            set-type-ppm-binary
            set-type-pam
            rewind-port))



(define (throw-unsupported-format ctx ch)
  (pnm-error "Unsupported format" ctx ch))



(define (set-type-pbm-ascii ctx)
  (context-result-set ctx 'pbm-ascii))

(define (set-type-pgm-ascii ctx)
  (context-result-set ctx 'pgm-ascii))

(define (set-type-ppm-ascii ctx)
  (context-result-set ctx 'ppm-ascii))

(define (set-type-pbm-binary ctx)
  (context-result-set ctx 'pbm-binary))

(define (set-type-pgm-binary ctx)
  (context-result-set ctx 'pgm-binary))

(define (set-type-ppm-binary ctx)
  (context-result-set ctx 'ppm-binary))

(define (set-type-pam ctx)
  (context-result-set ctx 'pam))



(define (rewind-port ctx)
  (let ((buffer (context-buffer ctx))
        (port   (context-port ctx)))
    (for-each (lambda (ch) (unget-char port ch)) buffer))
  ctx)

;; pnm-context.scm ends here.
