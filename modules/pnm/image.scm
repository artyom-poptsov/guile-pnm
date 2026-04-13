(define-module (pnm image)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (pnm core common)
  #:use-module (pnm core error)
  #:export (<pnm-image>
            pnm-image-commentary
            pnm-image-commentary-set!
            pnm-image-width
            pnm-image-width-set!
            pnm-image-height
            pnm-image-height-set!
            pnm-image-data
            pnm-image-data-set!

            <pbm-image>
            <pbm-binary-image>
            <pgm-image>
            <pgm-binary-image>
            pnm-image-grayscale-maximum-value
            pnm-image-grayscale-maximum-value-set!

            <ppm-image>
            <ppm-binary-image>
            pnm-image-color-maximum-value
            pnm-image-color-maximum-value-set!

            pnm-image->pnm))


(define-class <pnm-image> ()
  ;; <string> | #f
  (commentary
   #:init-value   #f
   #:init-keyword #:commentary
   #:getter       pnm-image-commentary
   #:setter       pnm-image-commentary-set!)

  ;; <number> | #f
  (width
   #:init-value   #f
   #:init-keyword #:width
   #:getter       pnm-image-width
   #:setter       pnm-image-width-set!)

  ;; <number> | #f
  (height
   #:init-value   #f
   #:init-keyword #:height
   #:getter       pnm-image-height
   #:setter       pnm-image-height-set!)

  ;; <vector>
  (data
   #:init-value   #()
   #:init-keyword #:data
   #:getter       pnm-image-data
   #:setter       pnm-image-data-set!))

(define-method (initialize (image <pnm-image>) initargs)
  (next-method)
  (let ((data   (constructor-argument #:data initargs))
        (width  (constructor-argument #:width initargs))
        (height (constructor-argument #:height initargs)))
    ;; Check the width value.
    (unless (number? width)
      (pnm-error "Width must be a number" initargs width))
    (unless (> width 0)
      (pnm-error "Width must be greater than zero" initargs width))
    ;; Check the height value.
    (unless (number? height)
      (pnm-error "Height must be a number" initargs height))
    (unless (> height 0)
      (pnm-error "Height must be greater than zero" initargs height))
    ;; Check the data.
    (unless data
      (pnm-error "Data must be specified" initargs data))
    (unless (> (vector-length data) 0)
      (pnm-error "Data must be a vector of a non-zero length"
                 initargs
                 data))))


(define-class <pbm-image> (<pnm-image>))

(define-method (pnm-image->pnm (image <pbm-image>) (port <port>))
  (let* ((width       (pnm-image-width image))
         (height      (pnm-image-height image))
         (data        (pnm-image-data image))
         (data-length (vector-length data)))
    (format port "P1~%")
    (when (pnm-image-commentary image)
      (format port "# ~a~%" (pnm-image-commentary image)))
    (format port "~a ~a~%" width height)
    (let loop ((index  0)
               (row    0)
               (column 0))
      (when (< index data-length)
        (format port "~a" (vector-ref data index))
        (if (= column (- width 1))
            (begin
              (newline port)
              (loop (+ index 1)
                    (+ row 1)
                    0))
            (begin
              (display " " port)
              (loop (+ index 1)
                    row
                    (+ column 1))))))))

(define-class <pbm-binary-image> (<pbm-image>))

(define-method (pnm-image->pnm (image <pbm-binary-image>) (port <port>))
  (let* ((width       (pnm-image-width image))
         (height      (pnm-image-height image))
         (data        (pnm-image-data image))
         (data-length (vector-length data)))
    (format port "P4~%")
    (when (pnm-image-commentary image)
      (format port "# ~a~%" (pnm-image-commentary image)))
    (format port "~a ~a~%" width height)
    (put-bytevector port (u8-list->bytevector (vector->list data)))))



(define-class <pgm-image> (<pnm-image>)
  ;; <number>
  (grayscale-maximum-value
   #:init-value  16
   #:init-keyword #:grayscale-maxiumum-value
   #:getter       pnm-image-grayscale-maximum-value
   #:setter       pnm-image-grayscale-maximum-value-set!))

(define-method (pnm-image->pnm (image <pgm-image>) (port <port>))
  (let* ((width       (pnm-image-width image))
         (height      (pnm-image-height image))
         (grayscale   (pnm-image-grayscale-maximum-value image))
         (data        (pnm-image-data image))
         (data-length (vector-length data)))
    (format port "P2~%")
    (when (pnm-image-commentary image)
      (format port "# ~a~%" (pnm-image-commentary image)))
    (format port "~a ~a~%" width height)
    (format port "~a~%" grayscale)
    (let loop ((index  0)
               (row    0)
               (column 0))
      (when (< index data-length)
        (format port "~3a" (vector-ref data index))
        (if (= column (- width 1))
            (begin
              (newline port)
              (loop (+ index 1)
                    (+ row 1)
                    0))
            (begin
              (display " " port)
              (loop (+ index 1)
                    row
                    (+ column 1))))))))

(define-class <pgm-binary-image> (<pgm-image>))

(define-method (pnm-image->pnm (image <pgm-binary-image>) (port <port>))
  (let* ((width       (pnm-image-width image))
         (height      (pnm-image-height image))
         (grayscale   (pnm-image-grayscale-maximum-value image))
         (data        (pnm-image-data image))
         (data-length (vector-length data)))
    (format port "P5~%")
    (when (pnm-image-commentary image)
      (format port "# ~a~%" (pnm-image-commentary image)))
    (format port "~a ~a~%" width height)
    (format port "~a~%" grayscale)
    (put-bytevector port (u8-list->bytevector (vector->list data)))))


(define-class <ppm-image> (<pnm-image>)
  ;; <number>
  (color-maximum-value
   #:init-value  255
   #:init-keyword #:color-maxiumum-value
   #:getter       pnm-image-color-maximum-value
   #:setter       pnm-image-color-maximum-value-set!))

(define-method (pnm-image->pnm (image <ppm-image>) (port <port>))
  (let* ((width       (pnm-image-width image))
         (height      (pnm-image-height image))
         (color       (pnm-image-color-maximum-value image))
         (data        (pnm-image-data image))
         (data-length (vector-length data)))
    (format port "P3~%")
    (when (pnm-image-commentary image)
      (format port "# ~a~%" (pnm-image-commentary image)))
    (format port "~a ~a~%" width height)
    (format port "~a~%" color)
    (let loop ((index  0)
               (row    0)
               (column 0))
      (when (< index data-length)
        (format port "~3a" (vector-ref data index))
        (if (= column (- width 1))
            (begin
              (newline port)
              (loop (+ index 1)
                    (+ row 1)
                    0))
            (begin
              (display " " port)
              (loop (+ index 1)
                    row
                    (+ column 1))))))))

(define-class <ppm-binary-image> (<ppm-image>))

(define-method (pnm-image->pnm (image <ppm-binary-image>) (port <port>))
  (let* ((width       (pnm-image-width image))
         (height      (pnm-image-height image))
         (color       (pnm-image-color-maximum-value image))
         (data        (pnm-image-data image))
         (data-length (vector-length data)))
    (format port "P6~%")
    (when (pnm-image-commentary image)
      (format port "# ~a~%" (pnm-image-commentary image)))
    (format port "~a ~a~%" width height)
    (format port "~a~%" color)
    (put-bytevector port (u8-list->bytevector (vector->list data)))))



;; image.scm ends here.
