(define-module (tests common)
  #:use-module (oop goops)
  #:use-module (pnm fsm context)
  #:export (configure-test-logging!
            %topdir
            %test-p1-file
            %test-p2-file
            %test-p3-file
            %test-p4-file
            %test-p5-file
            %test-p6-file))


(define-method (configure-test-logging! (test-suite-name <string>))
  (smc-log-init! "file" `((file . ,(string-append test-suite-name "-smc.log")))))



(define %topdir (getenv "abs_top_srcdir"))

(define %test-p1-file (format #f "~a/tests/image/p1.pbm" %topdir))
(define %test-p2-file (format #f "~a/tests/image/p2.pgm" %topdir))
(define %test-p3-file (format #f "~a/tests/image/p3.ppm" %topdir))
(define %test-p4-file (format #f "~a/tests/image/p4.pbm" %topdir))
(define %test-p5-file (format #f "~a/tests/image/p5.pgm" %topdir))
(define %test-p6-file (format #f "~a/tests/image/p6.ppm" %topdir))

;; common.scm ends here.
