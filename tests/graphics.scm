(use-modules (srfi srfi-64)
             (oop goops)
             (pnm image)
             (pnm graphics))

(define %test-name "graphics")

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


(test-begin %test-name)


;; Helper procedures.

(test-equal "cartesian->index"
  10
  (cartesian->index 6 4 1))

(test-assert "assert-index: <pbm-ascii-image>: success"
  (let ((image (make <pbm-ascii-image>
                 #:width  6
                 #:height 10
                 #:commentary "This is an example bitmap of the letter \"J\""
                 #:data %test-data)))
    (assert-index image 10)
    #t))

(test-error "assert-index: <pbm-ascii-image>: failure"
  'pnm-error
  (let ((image (make <pbm-ascii-image>
                 #:width  6
                 #:height 10
                 #:commentary "This is an example bitmap of the letter \"J\""
                 #:data %test-data)))
    (assert-index image 100)
    #t))

(test-assert "assert-pixel-value: <pbm-ascii-image>: success"
  (let ((image (make <pbm-ascii-image>
                 #:width  6
                 #:height 10
                 #:commentary "This is an example bitmap of the letter \"J\""
                 #:data %test-data)))
    (assert-pixel-value image 1)
    #t))

(test-error "assert-pixel-value: <pbm-ascii-image>: failure"
  'pnm-error
  (let ((image (make <pbm-ascii-image>
                 #:width  6
                 #:height 10
                 #:commentary "This is an example bitmap of the letter \"J\""
                 #:data %test-data)))
    (assert-pixel-value image 10)
    #t))

(test-assert "assert-pixel-value: <pgm-ascii-image>: success"
  (let ((image (make <pgm-ascii-image>
                 #:width  6
                 #:height 10
                 #:grayscale-maxiumum-value 16
                 #:data %test-data)))
    (assert-pixel-value image 8)
    #t))

(test-error "assert-pixel-value: <pgm-ascii-image>: failure"
  'pnm-error
  (let ((image (make <pbm-ascii-image>
                 #:width  6
                 #:height 10
                 #:grayscale-maxiumum-value 16
                 #:data %test-data)))
    (assert-pixel-value image 32)
    #t))

(test-assert "assert-pixel-value: <ppm-ascii-image>: success"
  (let ((image (make <ppm-ascii-image>
                 #:width  6
                 #:height 10
                 #:grayscale-maxiumum-value 16
                 #:data %test-data)))
    (assert-pixel-value image #(8 8 8))
    #t))

(test-error "assert-pixel-value: <ppm-ascii-image>: failure (vector length)"
  'pnm-error
  (let ((image (make <ppm-ascii-image>
                 #:width  6
                 #:height 10
                 #:color-maxiumum-value 16
                 #:data %test-data)))
    (assert-pixel-value image #(8))
    #t))

(test-error "assert-pixel-value: <ppm-ascii-image>: failure (color depth)"
  'pnm-error
  (let ((image (make <ppm-ascii-image>
                 #:width  6
                 #:height 10
                 #:color-maxiumum-value 16
                 #:data %test-data)))
    (assert-pixel-value image #(32 32 32))
    #t))


;; Pixel manipulation.

(test-equal "pnm-image-pixel: <pbm-ascii-image>: index"
  1
  (let ((img (make <pbm-ascii-image>
               #:width  6
               #:height 10
               #:commentary "This is an example bitmap of the letter \"J\""
               #:data %test-data)))
    (pnm-image-pixel img 10)))

(test-equal "pnm-image-pixel: <pbm-binary-image>: index"
  1
  (let ((img (make <pbm-binary-image>
               #:width  4
               #:height 4
               #:data #(#b00000000 #b00100000))))
    (pnm-image-pixel img 10)))

(test-equal "pnm-image-pixel: <pbm-binary-image>: x, y"
  1
  (let ((img (make <pbm-binary-image>
               #:width  4
               #:height 4
               #:data #(#b00000000 #b00100000))))
    (pnm-image-pixel img 2 2)))

(test-equal "pnm-image-pixel: <pbm-ascii-image>: x, y"
  1
  (let ((img (make <pbm-ascii-image>
               #:width  6
               #:height 10
               #:commentary "This is an example bitmap of the letter \"J\""
               #:data %test-data)))
    (pnm-image-pixel img 4 1)))

(test-equal "pnm-image-pixel-set!: <pbm-ascii-image>: index"
  0
  (let ((image (make <pbm-ascii-image>
                 #:width  6
                 #:height 10
                 #:commentary "This is an example bitmap of the letter \"J\""
                 #:data (vector-copy %test-data))))
    (pnm-image-pixel-set! image 10 0)
    (pnm-image-pixel image 4 1)))

(test-equal "pnm-image-pixel-set!: <pbm-ascii-image>: x, y"
  0
  (let ((image (make <pbm-ascii-image>
                 #:width  6
                 #:height 10
                 #:commentary "This is an example bitmap of the letter \"J\""
                 #:data (vector-copy %test-data))))
    (pnm-image-pixel-set! image 4 1 0)
    (pnm-image-pixel image 4 1)))

(test-equal "pnm-image-pixel-set!: <pbm-binary-image>: index"
  #b00100000
  (let ((image (make <pbm-binary-image>
                 #:width  4
                 #:height 4
                 #:data #(#b00000000 #b00000000))))
    (pnm-image-pixel-set! image 10 0)
    (vector-ref (pnm-image-data image) 1)))

(test-equal "pnm-image-pixel-set!: <pbm-binary-image>: x, y"
  #b00100000
  (let ((image (make <pbm-binary-image>
                 #:width  4
                 #:height 4
                 #:data #(#b00000000 #b00000000))))
    (pnm-image-pixel-set! image 10 1)
    (vector-ref (pnm-image-data image) 1)))

(test-equal "pnm-image-pixel: <pgm-ascii-image>: index"
  #x60
  (let ((image (make <pgm-ascii-image>
                 #:grayscale-maxiumum-value 255
                 #:width  4
                 #:height 2
                 #:data (list->vector
                         (list #x10 #x20 #x30 #x40
                               #x50 #x60 #x70 #x80)))))
    (pnm-image-pixel image 5)))

(test-equal "pnm-image-pixel: <pgm-ascii-image>: x, y"
  #x60
  (let ((image (make <pgm-ascii-image>
                 #:grayscale-maxiumum-value 255
                 #:width  4
                 #:height 2
                 #:data (list->vector
                         (list #x10 #x20 #x30 #x40
                               #x50 #x60 #x70 #x80)))))
    (pnm-image-pixel image 1 1)))

(test-equal "pnm-image-pixel: <pgm-binary-image>: index"
  #x60
  (let ((image (make <pgm-binary-image>
                 #:grayscale-maxiumum-value 255
                 #:width  4
                 #:height 2
                 #:data (list->vector
                         (list #x10 #x20 #x30 #x40
                               #x50 #x60 #x70 #x80)))))
    (pnm-image-pixel image 5)))

(test-equal "pnm-image-pixel: <pgm-binary-image>: x, y"
  #x60
  (let ((image (make <pgm-binary-image>
                 #:grayscale-maxiumum-value 255
                 #:width  4
                 #:height 2
                 #:data (list->vector
                         (list #x10 #x20 #x30 #x40
                               #x50 #x60 #x70 #x80)))))
    (pnm-image-pixel image 1 1)))

(test-equal "pnm-image-pixel: <ppm-ascii-image>: index"
  #(#x00 #xFF #x00)
  (let ((image (make <ppm-ascii-image>
                 #:color-maxiumum-value 255
                 #:width  2
                 #:height 2
                 #:data (list->vector
                         (list #xFF #x00 #x00 #x00 #xFF #x00
                               #x00 #x00 #xFF #xFF  #xFF #xFF)))))
    (pnm-image-pixel image 1)))

(test-equal "pnm-image-pixel: <ppm-ascii-image>: x, y"
  #(#x00 #xFF #x00)
  (let ((image (make <ppm-ascii-image>
                 #:color-maxiumum-value 255
                 #:width  2
                 #:height 2
                 #:data (list->vector
                         (list #xFF #x00 #x00 #x00 #xFF #x00
                               #x00 #x00 #xFF #xFF  #xFF #xFF)))))
    (pnm-image-pixel image 1 0)))

(test-equal "pnm-image-pixel: <ppm-binary-image>: index"
  #(#x00 #xFF #x00)
  (let ((image (make <ppm-binary-image>
                 #:color-maxiumum-value 255
                 #:width  2
                 #:height 2
                 #:data (list->vector
                         (list #xFF #x00 #x00 #x00 #xFF #x00
                               #x00 #x00 #xFF #xFF  #xFF #xFF)))))
    (pnm-image-pixel image 1)))

(test-equal "pnm-image-pixel: <ppm-binary-image>: x, y"
  #(#x00 #xFF #x00)
  (let ((image (make <ppm-binary-image>
                 #:color-maxiumum-value 255
                 #:width  2
                 #:height 2
                 #:data (list->vector
                         (list #xFF #x00 #x00 #x00 #xFF #x00
                               #x00 #x00 #xFF #xFF  #xFF #xFF)))))
    (pnm-image-pixel image 1 0)))

(test-equal "pnm-image-pixel-set!: <pgm-ascii-image>: index"
  #xFF
  (let ((image (make <pgm-ascii-image>
                 #:grayscale-maxiumum-value 255
                 #:width  4
                 #:height 2
                 #:data (list->vector
                         (list #x10 #x20 #x30 #x40
                               #x50 #x60 #x70 #x80)))))
    (pnm-image-pixel-set! image 5 #xFF)
    (pnm-image-pixel image 5)))

(test-equal "pnm-image-pixel-set!: <pgm-ascii-image>: x, y"
  #xFF
  (let ((image (make <pgm-ascii-image>
                 #:grayscale-maxiumum-value 255
                 #:width  4
                 #:height 2
                 #:data (list->vector
                         (list #x10 #x20 #x30 #x40
                               #x50 #x60 #x70 #x80)))))
    (pnm-image-pixel-set! image 1 1 #xFF)
    (pnm-image-pixel image 5)))

(test-equal "pnm-image-pixel-set!: <pgm-ascii-image>: x, y"
  #xFF
  (let ((image (make <pgm-ascii-image>
                 #:grayscale-maxiumum-value 255
                 #:width  4
                 #:height 2
                 #:data (list->vector
                         (list #x10 #x20 #x30 #x40
                               #x50 #x60 #x70 #x80)))))
    (pnm-image-pixel-set! image 1 1 #xFF)
    (pnm-image-pixel image 5)))

(test-equal "pnm-image-pixel-set!: <pgm-binary-image>: index"
  #xFF
  (let ((image (make <pgm-binary-image>
                 #:grayscale-maxiumum-value 255
                 #:width  4
                 #:height 2
                 #:data (list->vector
                         (list #x10 #x20 #x30 #x40
                               #x50 #x60 #x70 #x80)))))
    (pnm-image-pixel-set! image 5 #xFF)
    (pnm-image-pixel image 5)))

(test-equal "pnm-image-pixel-set!: <pgm-binary-image>: x, y"
  #xFF
  (let ((image (make <pgm-binary-image>
                 #:grayscale-maxiumum-value 255
                 #:width  4
                 #:height 2
                 #:data (list->vector
                         (list #x10 #x20 #x30 #x40
                               #x50 #x60 #x70 #x80)))))
    (pnm-image-pixel-set! image 1 1 #xFF)
    (pnm-image-pixel image 5)))

(test-equal "pnm-image-pixel-set!: <ppm-ascii-image>: index"
  #(#xAA #xAA #xAA)
  (let ((image (make <ppm-ascii-image>
                 #:color-maxiumum-value 255
                 #:width  2
                 #:height 2
                 #:data (list->vector
                         (list #xFF #x00 #x00 #x00 #xFF #x00
                               #x00 #x00 #xFF #xFF #xFF #xFF)))))
    (pnm-image-pixel-set! image 1 #(#xAA #xAA #xAA))
    (pnm-image-pixel image 1)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit exit-status)

;; graphics.scm ends here.
