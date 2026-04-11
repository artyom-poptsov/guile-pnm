(define-module (pnm fsm pbm-context)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-1)
  #:use-module (scheme documentation)
  #:use-module (pnm fsm context)
  #:use-module (pnm core error)
  #:export (throw-magic-number-error
            throw-unexpected-eof
            throw-format-error
            pbm-init-result
            pbm-set-data
            pbm-set-width
            pbm-set-height
            pbm-append-comment
            pbm-validate-image
            debug))



(define (throw-magic-number-error ctx ch)
  (pnm-error "Magic number error" ctx ch))

(define (throw-unexpected-eof ctx ch)
  (pnm-error "Unexpected end of file" ctx ch))

(define (throw-format-error ctx ch)
  (pnm-error "Format error" ctx ch))



(define (pbm-init-result ctx)
  (context-result-set ctx '((comment . #f)
                            (width   . #f)
                            (height  . #f)
                            (data    . #f))))

(define (pbm-set-width ctx pnm)
  (let* ((result (context-result ctx))
         (buffer (list->string (context-buffer/reversed ctx)))
         (width  (string->number buffer)))
    (clear-buffer
     (context-result-set ctx
                         (acons 'width width
                                (alist-delete 'width result))))))

(define (pbm-set-height ctx pnm)
  (let* ((result (context-result ctx))
         (buffer (list->string (context-buffer/reversed ctx)))
         (height (string->number buffer)))
    (clear-buffer
     (context-result-set ctx
                         (acons 'height height
                                (alist-delete 'height result))))))

(define (pbm-append-comment ctx ch)
  (let* ((result      (context-result ctx))
         (buffer      (context-buffer/reversed ctx))
         (new-comment (string-trim-both (list->string buffer)))
         (pbm-comment (if (assoc-ref result 'comment)
                          (assoc-ref result 'comment)
                          ""))
         (comment     (string-append pbm-comment new-comment)))
    (clear-buffer
     (context-result-set ctx
                         (acons 'comment comment
                                (alist-delete 'comment result))))))

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
