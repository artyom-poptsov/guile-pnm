(define-module (pnm fsm p3-context)
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
  #:export (ppm-init-result
            ppm-set-data
            ppm-set-color
            ppm-validate-image))



(define (ppm-init-result ctx)
  (context-result-set ctx '((comment   . #f)
                            (width     . #f)
                            (height    . #f)
                            (color     . #f)
                            (data      . #f))))

(define (ppm-stanza->data stanza)
  (list->vector
   (reverse (map (lambda (element)
                   (string->number
                    (list->string (reverse element))))
                 stanza))))

(define (ppm-set-data ctx pnm)
  (let* ((result (context-result ctx))
         (stanza (context-stanza ctx))
         (buffer (context-buffer ctx))
         (data
          (if (null? buffer)
              (ppm-stanza->data stanza)
              (ppm-stanza->data (cons buffer stanza)))))
    (clear-stanza
     (context-result-set ctx
                         (acons 'data data
                                (alist-delete 'data result))))))

(define (ppm-set-color ctx pnm)
  (let* ((result (context-result ctx))
         (buffer (list->string (context-buffer/reversed ctx)))
         (color  (string->number buffer)))
    (clear-buffer
     (context-result-set ctx
                         (acons 'color color
                                (alist-delete 'color result))))))

(define (ppm-validate-image ctx)
  ctx)

;; pgm-context.scm ends here.
