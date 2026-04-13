(define-module (pnm fsm p6-context)
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
  #:export (p6-init-result
            p6-set-data
            p6-validate-image
            set-color))



(define (p6-init-result ctx)
  (context-result-set ctx '((comment   . #f)
                            (width     . #f)
                            (height    . #f)
                            (grayscale . #f)
                            (data      . #f))))

(define (p6-set-data ctx pnm)
  (let* ((result (context-result ctx))
         (data   (list->vector (reverse (context-buffer ctx)))))
    (clear-stanza
     (context-result-set ctx
                         (acons 'data data
                                (alist-delete 'data result))))))

(define (set-color ctx pnm)
  (let* ((result (context-result ctx))
         (buffer (list->string (map integer->char
                                    (context-buffer/reversed ctx))))
         (grayscale (string->number buffer)))
    (clear-buffer
     (context-result-set ctx
                         (acons 'grayscale grayscale
                                (alist-delete 'grayscale result))))))

(define (p6-validate-image ctx)
  ctx)

;; pgm-context.scm ends here.
