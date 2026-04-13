(define-module (pnm fsm p1-context)
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
  (let* ((result     (context-result ctx))
         (width      (assoc-ref result 'width))
         (height     (assoc-ref result 'height))
         (commentary (assoc-ref result 'comment))
         (data       (assoc-ref result 'data)))
    (unless (> width 0)
      (pnm-error "Width must be greater than zero" result width))
    (unless (> height 0)
      (pnm-error "Height must be greater than zero" result height))
    (unless data
      (pnm-error "Data must be set" result data))
    (unless (> (vector-length data) 0)
      (pnm-error "Data must be a vector with non-zero length" result data))
    ctx))

;; pbm-context.scm ends here.
