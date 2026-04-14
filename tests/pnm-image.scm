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
   "\n")
  (let ((img (make <pbm-ascii-image>
               #:width  6
               #:height 10
               #:commentary "This is an example bitmap of the letter \"J\""
               #:data %test-data)))
    (with-output-to-string
      (lambda ()
        (pnm-image->pnm img (current-output-port))))))

;; Pixel manipulation.

(test-equal "cartesian->index"
  10
  (cartesian->index 6 4 1))

(test-assert "assert-index: success"
  (let ((image (make <pbm-ascii-image>
                 #:width  6
                 #:height 10
                 #:commentary "This is an example bitmap of the letter \"J\""
                 #:data %test-data)))
    (assert-index image 10)
    #t))

(test-error "assert-index: failure"
  'pnm-error
  (let ((image (make <pbm-ascii-image>
                 #:width  6
                 #:height 10
                 #:commentary "This is an example bitmap of the letter \"J\""
                 #:data %test-data)))
    (assert-index image 100)
    #t))

(test-equal "pnm-image-pixel: index"
  1
  (let ((img (make <pbm-ascii-image>
               #:width  6
               #:height 10
               #:commentary "This is an example bitmap of the letter \"J\""
               #:data %test-data)))
    (pnm-image-pixel img 10)))

(test-equal "pnm-image-pixel: x, y"
  1
  (let ((img (make <pbm-ascii-image>
               #:width  6
               #:height 10
               #:commentary "This is an example bitmap of the letter \"J\""
               #:data %test-data)))
    (pnm-image-pixel img 4 1)))


;; PGM
(test-equal "make <pgm-ascii-image>"
  (string-append
   (string-join
    (list "P2"
          "2 4"
          "8"
          "0   1  "
          "2   3  "
          "4   5  "
          "6   7  ")
    "\n")
   "\n")
  (with-output-to-string
    (lambda ()
      (pnm-image->pnm (make <pgm-ascii-image>
                        #:width  2
                        #:height 4
                        #:grayscale-maxiumum-value 8
                        #:data   #(0 1 2 3 4 5 6 7))
                      (current-output-port)))))


(define %test-ppm-data
  (list->vector
   (list
    255 0   0
    0   255 0
    0   0   255
    255 255 0
    255 255 255
    0   0   0)))

(test-equal "pnm-image->pnm: PPM"
  (string-append
   (string-join
    (list
     "P3"
     "# This is a commentary."
     "3 2"
     "255"
     "255 0   0  "
     "0   255 0  "
     "0   0   255"
     "255 255 0  "
     "255 255 255"
     "0   0   0  ")
    "\n")
   "\n")
  (let ((img (make <ppm-ascii-image>
               #:width  3
               #:height 2
               #:commentary "This is a commentary."
               #:data %test-ppm-data)))
    (with-output-to-string
      (lambda ()
        (pnm-image->pnm img (current-output-port))))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)
