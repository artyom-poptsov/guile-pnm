(define-module (pnm fsm p4-context)
  #:use-module (srfi srfi-1)
  #:use-module (pnm fsm common)
  #:use-module (pnm fsm u8-context)
  #:use-module (pnm core error)
  #:re-export (throw-magic-number-error
               throw-unexpected-eof
               throw-format-error
               pnm-append-comment
               set-width/binary
               set-height/binary
               append-comment/binary
               none)
  #:export (p4-init-result
            p4-set-data
            p4-validate-image))



(define (p4-init-result ctx)
  (context-result-set ctx '((comment   . #f)
                            (width     . #f)
                            (height    . #f)
                            (data      . #f))))

(define (stanza->data stanza)
  (list->vector
   (reverse (map (lambda (element)
                   (string->number
                    (list->string (reverse element))))
                 stanza))))

(define (p4-set-data ctx pnm)
  (let* ((result (context-result ctx))
         (stanza (context-stanza ctx))
         (buffer (context-buffer ctx))
         (data
          (if (null? buffer)
              (stanza->data stanza)
              (stanza->data (cons buffer stanza)))))
    (clear-stanza
     (context-result-set ctx
                         (acons 'data data
                                (alist-delete 'data result))))))

(define (p4-validate-image ctx)
  ctx)

;; pgm-context.scm ends here.
