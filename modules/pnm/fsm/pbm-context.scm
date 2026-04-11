(define-module (pnm fsm pbm-context)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-1)
  #:use-module (scheme documentation)
  #:use-module (pnm fsm common)
  #:use-module (pnm fsm context)
  #:use-module (pnm core error)
  #:re-export (throw-magic-number-error
               throw-unexpected-eof
               throw-format-error
               pnm-append-comment
               pnm-set-width
               pnm-set-height)
  #:export (pbm-init-result
            pbm-set-data
            pbm-validate-image
            debug))



(define (pbm-init-result ctx)
  (context-result-set ctx '((comment . #f)
                            (width   . #f)
                            (height  . #f)
                            (data    . #f))))

(define (pbm-stanza->data stanza)
  (list->vector
   (reverse (map (lambda (element)
                   (string->number
                    (list->string (reverse element))))
                 stanza))))

(define (pbm-set-data ctx pnm)
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

(define (pbm-validate-image ctx)
  ctx)

;; pbm-context.scm ends here.
