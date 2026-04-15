;;; image.scm -- Guile PNM image types.

;; Copyright © 2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This module contains PNM image classes (types) and their methods.


;;; Code:

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

            <pbm-ascii-image>
            <pbm-binary-image>
            <pgm-ascii-image>
            <pgm-binary-image>
            pnm-image-grayscale-maximum-value
            pnm-image-grayscale-maximum-value-set!

            <ppm-ascii-image>
            <ppm-binary-image>
            pnm-image-color-maximum-value
            pnm-image-color-maximum-value-set!

            pnm-image->pnm

            ;; Converters.
            pbm-ascii-image->pbm-binary-image
            pbm-binary-image->pbm-ascii-image

            ;; Basic pixel manipulation.
            cartesian->index
            assert-index
            pnm-image-pixel))


;; Helper procedures.

(define-method (pnm-print-commentary (port <port>) commentary)
  (when commentary
    (format port "# ~a~%" commentary)))

(define-method (pnm-print-dimensions (port <port>)
                                     (width <number>)
                                     (height <number>))
  (format port "~a ~a~%" width height))


(define-class <pnm-image> ()
  ;; Image commentary.
  ;;
  ;; <string> | #f
  (commentary
   #:init-value   #f
   #:init-keyword #:commentary
   #:getter       pnm-image-commentary
   #:setter       pnm-image-commentary-set!)

  ;; REQUIRED.  Image width.
  ;;
  ;; <number>
  (width
   #:init-value   #f
   #:init-keyword #:width
   #:getter       pnm-image-width
   #:setter       pnm-image-width-set!)

  ;; REQUIRED.  Image height.
  ;;
  ;; <number>
  (height
   #:init-value   #f
   #:init-keyword #:height
   #:getter       pnm-image-height
   #:setter       pnm-image-height-set!)

  ;; REQUIRED.  Image data as a vector of numbers.
  ;;
  ;; <vector>
  (data
   #:init-value   #()
   #:init-keyword #:data
   #:getter       pnm-image-data
   #:setter       pnm-image-data-set!))

(define-method (initialize (image <pnm-image>) initargs)
  "PNM image constructor which ensures that all the required object fields are
set and have a proper value."
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



(define-method (%display (image <pnm-image>) (port <port>))
  (format port "#<pnm-image width: ~a height: ~a ~a>"
          (pnm-image-width image)
          (pnm-image-height image)
          (object-address/hex-string image)))

(define-method (display (image <pnm-image>) (port <port>))
  (%display image port))

(define-method (write (image <pnm-image>) (port <port>))
  (%display image port))





(define-class <pbm-ascii-image> (<pnm-image>))

(define-method (pnm-image->pnm (image <pbm-ascii-image>) (port <port>))
  "Convert an @var{image} into a PNM file."
  (let* ((width       (pnm-image-width image))
         (height      (pnm-image-height image))
         (data        (pnm-image-data image))
         (data-length (vector-length data)))
    (format port "P1~%")
    (pnm-print-commentary port (pnm-image-commentary image))
    (pnm-print-dimensions port width height)
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


(define-method (pbm-ascii-image->pbm-binary-image (image <pbm-ascii-image>))
  "Convert an ASCII (plain) PBM @var{image} to a binary PBM image.  Return a new
@code{<pbm-binary-image>} instance."
  (define (ascii-data->binary-data data width)
    (let ((data-length (vector-length data)))
      (let loop ((index      0)
                 (column     0)
                 (byte-index 7)
                 (byte       0)
                 (result     '()))
        (if (< index data-length)
            (let* ((value (vector-ref data index))
                   (byte  (logior byte (ash value byte-index))))
              (cond
               ((= column (- width 1))
                (loop (+ index 1)
                      0 ; column
                      7 ; byte-index
                      0 ; byte
                      (cons byte result)))
               ((> byte-index 0)
                (loop (+ index 1)
                      (+ column 1)
                      (- byte-index 1)
                      byte
                      result))
               ((zero? byte-index)
                (loop (+ index 1)
                      (+ column 1)
                      7 ; byte-index
                      0 ; byte
                      (cons byte result)))
               ((= column width)
                (loop (+ index 1)
                      0 ; column
                      7 ; byte-index
                      0 ; byte
                      (cons byte result)))
               (else
                (loop (+ index 1)
                      (+ column 1)
                      (- byte-index 1)
                      byte
                      result))))
            (list->vector (reverse result))))))

  (let* ((width       (pnm-image-width image))
         (height      (pnm-image-height image))
         (commentary  (pnm-image-commentary image))
         (data        (pnm-image-data image))
         (binary-data (ascii-data->binary-data data width)))
    (make <pbm-binary-image>
      #:width width
      #:height height
      #:commentary commentary
      #:data binary-data)))



(define-method (%display (image <pbm-ascii-image>) (port <port>))
  (format port "#<pbm-ascii-image width: ~a height: ~a ~a>"
          (pnm-image-width image)
          (pnm-image-height image)
          (object-address/hex-string image)))

(define-class <pbm-binary-image> (<pbm-ascii-image>))

(define-method (%display (image <pbm-binary-image>) (port <port>))
  (format port "#<pbm-binary-image width: ~a height: ~a ~a>"
          (pnm-image-width image)
          (pnm-image-height image)
          (object-address/hex-string image)))

(define-method (pnm-image->pnm (image <pbm-binary-image>) (port <port>))
  (let* ((width       (pnm-image-width image))
         (height      (pnm-image-height image))
         (data        (pnm-image-data image))
         (data-length (vector-length data)))
    (format port "P4~%")
    (pnm-print-commentary port (pnm-image-commentary image))
    (pnm-print-dimensions port width height)
    (put-bytevector port (u8-list->bytevector (vector->list data)))))

(define-method (pbm-binary-image->pbm-ascii-image (image <pbm-binary-image>))
  "Convert an binary PBM @var{image} to an ASCII (plain) PBM image.  Return a new
@code{<pbm-ascii-image>} instance."
  (define (binary-data->ascii-data data width)
    (let ((data-length (vector-length data))
          (extra-bits  (remainder width 8)))
      (let loop ((index      0)
                 (column     0)
                 (byte-index 7)
                 (result     '()))
        (if (< index data-length)
            (let* ((byte  (vector-ref data index))
                   (value (if (logbit? byte-index byte) 1 0)))
              (cond
               ((= column (- width 1))
                (loop (+ index 1)
                      0 ; column
                      7 ; byte-index
                      (cons value result)))
               ((> byte-index 0)
                (loop index
                      (+ column 1)
                      (- byte-index 1)
                      (cons value result)))
               ((zero? byte-index)
                (loop (+ index 1)
                      (+ column 1)
                      7 ; byte-index
                      (cons value result)))))
            (list->vector (reverse result))))))

  (let* ((width       (pnm-image-width image))
         (height      (pnm-image-height image))
         (commentary  (pnm-image-commentary image))
         (data        (pnm-image-data image))
         (data        (binary-data->ascii-data data width)))
    (make <pbm-ascii-image>
      #:width width
      #:height height
      #:commentary commentary
      #:data data)))



(define-class <pgm-ascii-image> (<pnm-image>)
  ;; REQUIRED.  Maximum grayscale value.
  ;;
  ;; <number>
  (grayscale-maximum-value
   #:init-value  16
   #:init-keyword #:grayscale-maxiumum-value
   #:getter       pnm-image-grayscale-maximum-value
   #:setter       pnm-image-grayscale-maximum-value-set!))

(define-method (initialize (image <pgm-ascii-image>) initargs)
  (next-method)
  (let ((grayscale (constructor-argument #:grayscale-maxiumum-value
                                         initargs)))
    (unless (number? grayscale)
      (pnm-error "Grayscale maximum value must be a number"
                 initargs grayscale))))

(define-method (%display (image <pgm-ascii-image>) (port <port>))
  (format port "#<pgm-ascii-image width: ~a height: ~a grayscale: ~a ~a>"
          (pnm-image-width image)
          (pnm-image-height image)
          (pnm-image-grayscale-maximum-value image)
          (object-address/hex-string image)))

(define-method (pnm-image->pnm (image <pgm-ascii-image>) (port <port>))
  (let* ((width       (pnm-image-width image))
         (height      (pnm-image-height image))
         (grayscale   (pnm-image-grayscale-maximum-value image))
         (data        (pnm-image-data image))
         (data-length (vector-length data)))
    (format port "P2~%")
    (pnm-print-commentary port (pnm-image-commentary image))
    (pnm-print-dimensions port width height)
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

(define-class <pgm-binary-image> (<pgm-ascii-image>))

(define-method (%display (image <pgm-binary-image>) (port <port>))
  (format port "#<pgm-binary-image width: ~a height: ~a grayscale: ~a ~a>"
          (pnm-image-width image)
          (pnm-image-height image)
          (pnm-image-grayscale-maximum-value image)
          (object-address/hex-string image)))

(define-method (pnm-image->pnm (image <pgm-binary-image>) (port <port>))
  (let* ((width       (pnm-image-width image))
         (height      (pnm-image-height image))
         (grayscale   (pnm-image-grayscale-maximum-value image))
         (data        (pnm-image-data image))
         (data-length (vector-length data)))
    (format port "P5~%")
    (pnm-print-commentary port (pnm-image-commentary image))
    (pnm-print-dimensions port width height)
    (format port "~a~%" grayscale)
    (put-bytevector port (u8-list->bytevector (vector->list data)))))


(define-class <ppm-ascii-image> (<pnm-image>)
  ;; REQUIRED.  Maximum color value.
  ;;
  ;; <number>
  (color-maximum-value
   #:init-value  255
   #:init-keyword #:color-maxiumum-value
   #:getter       pnm-image-color-maximum-value
   #:setter       pnm-image-color-maximum-value-set!))

(define-method (%display (image <ppm-ascii-image>) (port <port>))
  (format port "#<ppm-ascii-image width: ~a height: ~a color: ~a ~a>"
          (pnm-image-width image)
          (pnm-image-height image)
          (pnm-image-color-maximum-value image)
          (object-address/hex-string image)))

(define-method (pnm-image->pnm (image <ppm-ascii-image>) (port <port>))
  (let* ((width       (pnm-image-width image))
         (height      (pnm-image-height image))
         (color       (pnm-image-color-maximum-value image))
         (data        (pnm-image-data image))
         (data-length (vector-length data)))
    (format port "P3~%")
    (pnm-print-commentary port (pnm-image-commentary image))
    (pnm-print-dimensions port width height)
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

(define-class <ppm-binary-image> (<ppm-ascii-image>))

(define-method (%display (image <ppm-binary-image>) (port <port>))
  (format port "#<ppm-binary-image width: ~a height: ~a color: ~a ~a>"
          (pnm-image-width image)
          (pnm-image-height image)
          (pnm-image-color-maximum-value image)
          (object-address/hex-string image)))

(define-method (pnm-image->pnm (image <ppm-binary-image>) (port <port>))
  (let* ((width       (pnm-image-width image))
         (height      (pnm-image-height image))
         (color       (pnm-image-color-maximum-value image))
         (data        (pnm-image-data image))
         (data-length (vector-length data)))
    (format port "P6~%")
    (pnm-print-commentary port (pnm-image-commentary image))
    (pnm-print-dimensions port width height)
    (format port "~a~%" color)
    (put-bytevector port (u8-list->bytevector (vector->list data)))))



;; image.scm ends here.
