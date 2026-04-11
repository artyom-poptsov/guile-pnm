(define-module (pnm fsm pgm-context)
  #:use-module (srfi srfi-1)
  #:use-module (pnm fsm common)
  #:use-module (pnm fsm context)
  #:use-module (pnm core error)
  #:re-export (throw-magic-number-error
               throw-unexpected-eof
               throw-format-error
               pnm-append-comment
               pnm-set-width
               pnm-set-height)
  #:export (pgm-init-result
            pgm-set-data
            pgm-set-grayscale
            pgm-validate-image
            debug))



(define (pgm-init-result ctx)
  (context-result-set ctx '((comment   . #f)
                            (width     . #f)
                            (height    . #f)
                            (grayscale . #f)
                            (data      . #f))))

(define (pbm-stanza->data stanza)
  (list->vector
   (reverse (map (lambda (element)
                   (string->number
                    (list->string (reverse element))))
                 stanza))))

(define (pgm-set-data ctx pnm)
  (let* ((result (context-result ctx))
         (stanza (context-stanza ctx))
         (buffer (context-buffer ctx))
         (data
          (if (null? buffer)
              (pbm-stanza->data stanza)
              (pbm-stanza->data (cons buffer stanza)))))
    (clear-stanza
     (context-result-set ctx
                         (acons 'data data
                                (alist-delete 'data result))))))

(define (pgm-set-grayscale ctx pnm)
  (let* ((result (context-result ctx))
         (buffer (list->string (context-buffer/reversed ctx)))
         (grayscale (string->number buffer)))
    (clear-buffer
     (context-result-set ctx
                         (acons 'grayscale grayscale
                                (alist-delete 'grayscale result))))))

(define (pgm-validate-image ctx)
  ctx)

;; pgm-context.scm ends here.
