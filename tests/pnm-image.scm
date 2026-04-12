(use-modules (srfi srfi-64)
             (oop goops)
             (pnm image))

(define %test-name "pnm-image")


(test-begin %test-name)

(define %test-data
  (list->vector
   (list
    0 0 0 0 1 0
    0 0 0 0 1 0
    0 0 0 0 1 0
    0 0 0 0 1 0
    0 0 0 0 1 0
    0 0 0 0 1 0
    1 0 0 0 1 0
    0 1 1 1 0 0
    0 0 0 0 0 0
    0 0 0 0 0 0)))

(test-equal "pnm-image->pnm"
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
    "0 0 0 0 0 0\n")
   "\n")
  (let ((img (make <pbm-image>
               #:width  6
               #:height 10
               #:commentary "This is an example bitmap of the letter \"J\""
               #:data %test-data)))
    (with-output-to-string
      (lambda ()
        (pnm-image->pnm img (current-output-port))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
