(define-module (pnm fsm common)
  #:use-module (srfi srfi-1)
  #:use-module (pnm core error)
  #:use-module (pnm fsm context)
  #:export(throw-magic-number-error
           throw-unexpected-eof
           throw-format-error
           pnm-set-width
           pnm-set-height
           pnm-append-comment
           none))


;; Error handling.

(define (throw-magic-number-error ctx ch)
  (pnm-error "Magic number error" ctx ch))

(define (throw-unexpected-eof ctx ch)
  (pnm-error "Unexpected end of file" ctx ch))

(define (throw-format-error ctx ch)
  (pnm-error "Format error" ctx ch))


;; "none" event source

(define (none ctx)
  #f)



(define (pnm-set-width ctx pnm)
  (let* ((result (context-result ctx))
         (buffer (list->string (context-buffer/reversed ctx)))
         (width  (string->number buffer)))
    (clear-buffer
     (context-result-set ctx
                         (acons 'width width
                                (alist-delete 'width result))))))

(define (pnm-set-height ctx pnm)
  (let* ((result (context-result ctx))
         (buffer (list->string (context-buffer/reversed ctx)))
         (height (string->number buffer)))
    (clear-buffer
     (context-result-set ctx
                         (acons 'height height
                                (alist-delete 'height result))))))

(define (pnm-append-comment ctx ch)
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

;; common.scm ends here.
