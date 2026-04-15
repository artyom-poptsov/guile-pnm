(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (tests common)
             (pnm)
             (pnm fsm context)
             (pnm fsm pnm)
             (oop goops)
             (pnm image))

(define %test-name "pnm")

(configure-test-logging! %test-name)


(test-begin %test-name)


;; Check the FSM for format determination.

(test-equal "pbm-ascii: correct"
  'pbm-ascii
  (with-input-from-string
      "P1"
    (lambda ()
      (let* ((fsm (make <pnm-fsm> #:debug-mode? #t))
             (ctx (make-char-context #:port (current-input-port)))
             (ctx (fsm-run! fsm ctx)))
        (context-result ctx)))))

(test-equal "pgm-ascii: correct"
  'pgm-ascii
  (with-input-from-string
      "P2"
    (lambda ()
      (let* ((fsm (make <pnm-fsm> #:debug-mode? #t))
             (ctx (make-char-context #:port (current-input-port)))
             (ctx (fsm-run! fsm ctx)))
        (context-result ctx)))))

(test-equal "ppm-ascii: correct"
  'ppm-ascii
  (with-input-from-string
      "P3"
    (lambda ()
      (let* ((fsm (make <pnm-fsm> #:debug-mode? #t))
             (ctx (make-char-context #:port (current-input-port)))
             (ctx (fsm-run! fsm ctx)))
        (context-result ctx)))))

(test-equal "ppm-binary: correct"
  'pbm-binary
  (with-input-from-string
      "P4"
    (lambda ()
      (let* ((fsm (make <pnm-fsm> #:debug-mode? #t))
             (ctx (make-char-context #:port (current-input-port)))
             (ctx (fsm-run! fsm ctx)))
        (context-result ctx)))))

(test-equal "pgm-binary: correct"
  'pgm-binary
  (with-input-from-string
      "P5"
    (lambda ()
      (let* ((fsm (make <pnm-fsm> #:debug-mode? #t))
             (ctx (make-char-context #:port (current-input-port)))
             (ctx (fsm-run! fsm ctx)))
        (context-result ctx)))))

(test-equal "ppm-binary: correct"
  'ppm-binary
  (with-input-from-string
      "P6"
    (lambda ()
      (let* ((fsm (make <pnm-fsm> #:debug-mode? #t))
             (ctx (make-char-context #:port (current-input-port)))
             (ctx (fsm-run! fsm ctx)))
        (context-result ctx)))))

(test-equal "pam: correct"
  'pam
  (with-input-from-string
      "P7"
    (lambda ()
      (let* ((fsm (make <pnm-fsm> #:debug-mode? #t))
             (ctx (make-char-context #:port (current-input-port)))
             (ctx (fsm-run! fsm ctx)))
        (context-result ctx)))))


;; pnm-type

(test-equal "pnm-type: pbm-ascii"
  'pbm-ascii
  (with-input-from-string
      "P1"
    (lambda ()
      (pnm-type))))

(test-equal "pnm-type: #f"
  #f
  (with-input-from-string
      "PNG"
    (lambda ()
      (pnm-type))))



(define %test-pbm
  (string-append
   (string-join
    (list
     "P1"
     "# This is an example bitmap of the letter \"J\""
     "6 10"
     "0 0 0 0 1 0"
     "0 0 0 0 1 0"
     "0 0 0 0 1 0"
     "0 0 0 0 1 0"
     "0 0 0 0 1 0"
     "0 0 0 0 1 0"
     "1 0 0 0 1 0"
     "0 1 1 1 0 0"
     "0 0 0 0 0 0"
     "0 0 0 0 0 0")
    "\n")
   "\n"))

(test-equal "pnm->scm: p1"
  %test-pbm
  (let ((img (pnm->scm (open-input-file %test-p1-file)
                       #:debug-mode? #t)))
    (with-output-to-string
      (lambda ()
        (scm->pnm img (current-output-port))))))

(test-equal "pnm->scm: p2"
  (* 25 7)
  (let ((img (pnm->scm (open-input-file %test-p2-file)
                       #:debug-mode? #t)))
    (vector-length (pnm-image-data img))))

(test-equal "pnm->scm: p3"
  (* (* 3 2) ; pixel count
     3)      ; colors per pixel
  (let ((img (pnm->scm (open-input-file %test-p3-file)
                       #:debug-mode? #t)))
    (vector-length (pnm-image-data img))))

(test-equal "pnm->scm: p3 - data check"
  #(255 0 0 0 255 0 0 0 255 255 255 0 255 255 255 0 0 0)
  (let ((img (pnm->scm (open-input-file %test-p3-file)
                       #:debug-mode? #t)))
    (pnm-image-data img)))

(test-assert "pnm->scm: p4"
  (let ((img (pnm->scm (open-input-file %test-p4-file)
                       #:debug-mode? #t)))
    (with-output-to-string
      (lambda ()
        (scm->pnm img (current-output-port))))))

(test-assert "pnm->scm: p5"
  (let ((img (pnm->scm (open-input-file %test-p5-file)
                       #:debug-mode? #t)))
    (with-output-to-string
      (lambda ()
        (scm->pnm img (current-output-port))))))

(test-assert "pnm->scm: p6"
  (let ((img (pnm->scm (open-input-file %test-p6-file)
                       #:debug-mode? #t)))
    (with-output-to-string
      (lambda ()
        (scm->pnm img (current-output-port))))))


;; Converter tests.

(test-equal "ascii->binary, binary->ascii"
  %test-pbm
  (let* ((img (pnm->scm (open-input-file %test-p1-file)
                        #:debug-mode? #t))
         (bin-image (pbm-ascii-image->pbm-binary-image img))
         (ascii-image (pbm-binary-image->pbm-ascii-image bin-image)))
    (with-output-to-string
      (lambda ()
        (scm->pnm ascii-image (current-output-port))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
