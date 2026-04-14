;;; image.scm -- Guile PNM graphics module.

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

;; This module contains some graphic methods for PNM images.


;;; Code:

(define-module (pnm graphics)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (ice-9 binary-ports)
  #:use-module (pnm core common)
  #:use-module (pnm core error)
  #:use-module (pnm image)
  #:export (assert-index
            assert-pixel-value
            cartesian->index
            pnm-image-pixel
            pnm-image-pixel-set!))


;; Helper procedures.

(define-method (cartesian->index (image-width <number>)
                                 (x <number>)
                                 (y <number>))
  "Convert @var{x} and @var{y} coordinates on a Cartesian plane into a vector
index for a given @var{image-width}, return the index."
  (+ (* y image-width) x))

(define-method (assert-index (image <pnm-image>) (index <number>))
  "Assert that an @var{index} is in the bounds of the @var{image} data.  Throw
pnm-error if assertion fails.  Return value is undefined."
  (unless (and (>= index 0)
               (< index (* (pnm-image-width image) (pnm-image-height image))))
    (pnm-error "Pixel index out of bounds" image index)))

(define-method (assert-pixel-value (image <pbm-ascii-image>) (value <number>))
  "Assert that a pixel @var{value} is in the required bounds for an @var{image}.
Throw pnm-error of the assertion fails."
  (unless (or (= value 0) (= value 1))
    (pnm-error "Pixel must be either 0 or 1" image value)))

(define-method (assert-pixel-value (image <pbm-binary-image>) (value <number>))
  "Assert that a pixel @var{value} is in the required bounds for an @var{image}.
Throw pnm-error of the assertion fails."
  (unless (or (= value 0) (= value 1))
    (pnm-error "Pixel must be either 0 or 1" image value)))


;; Pixel manipulation.

(define-method (pnm-image-pixel (image <pbm-ascii-image>) (index <number>))
  "Get a pixel specified by an @var{index} from an @var{image}.  Return the pixel
as a number or throw a pnm-error on error."
  (assert-index image index)
  (vector-ref (pnm-image-data image) index))

(define-method (pnm-image-pixel (image <pbm-ascii-image>)
                                (x <number>)
                                (y <number>))
  "Get a pixel specified by @var{x} and @var{y} coordinates from an @var{image}.
Return the pixel as a number or throw a pnm-error on error."
  (pnm-image-pixel image (cartesian->index (pnm-image-width image) x y)))

(define-method (pnm-image-pixel-set! (image <pbm-ascii-image>)
                                     (index <number>)
                                     (value <number>))
  (assert-index image index)
  (assert-pixel-value image value)
  (vector-set! (pnm-image-data image) index value))

(define-method (pnm-image-pixel-set! (image <pbm-ascii-image>)
                                     (x <number>)
                                     (y <number>)
                                     (value <number>))
  "Get a pixel specified by @var{x} and @var{y} coordinates from an @var{image}.
Return the pixel as a number or throw a pnm-error on error."
  (pnm-image-pixel-set! image
                        (cartesian->index (pnm-image-width image) x y)
                        value))

;; graphics.scm ends here.


