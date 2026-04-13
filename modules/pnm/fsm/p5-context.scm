(define-module (pnm fsm p5-context)
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
  #:export (p5-init-result
            p5-set-data
            p5-validate-image
            set-grayscale))



(define (p5-init-result ctx)
  (context-result-set ctx '((comment   . #f)
                            (width     . #f)
                            (height    . #f)
                            (grayscale . #f)
                            (data      . #f))))

(define (p5-set-data ctx pnm)
  (let* ((result (context-result ctx))
         (data   (list->vector (reverse (context-buffer ctx)))))
    (clear-stanza
     (context-result-set ctx
                         (acons 'data data
                                (alist-delete 'data result))))))

(define (set-grayscale ctx pnm)
  (let* ((result (context-result ctx))
         (buffer (list->string (context-buffer/reversed ctx)))
         (grayscale (string->number buffer)))
    (clear-buffer
     (context-result-set ctx
                         (acons 'grayscale grayscale
                                (alist-delete 'grayscale result))))))

(define (p5-validate-image ctx)
  ctx)

;; pgm-context.scm ends here.
