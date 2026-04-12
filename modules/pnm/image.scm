(define-module (pnm image)
  #:use-module (oop goops)
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
            <pgm-image>
            pnm-image-grayscale-maximum-value
            pnm-image-grayscale-maximum-value-set!

            <ppm-image>
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
    (let loop ((index 0))
      (when (< index data-length)
        (format port "~a" (vector-ref data index))
        (if (zero? (remainder (+ index 1) width))
            (newline port)
            (display " " port))
        (loop (+ index 1))))))



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
    (let loop ((index 0))
      (when (< index data-length)
        (format port "~3a" (vector-ref data index))
        (if (zero? (remainder (+ index 1) width))
            (newline port)
            (display " " port))
        (loop (+ index 1))))
    (newline port)))


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
    (let loop ((index 0))
      (when (< index data-length)
        (format port "~3a" (vector-ref data index))
        (if (zero? (remainder (+ index 1) width))
            (newline port)
            (display " " port))
        (loop (+ index 1))))
    (newline port)))



;; image.scm ends here.
