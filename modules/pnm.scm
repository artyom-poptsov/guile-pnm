;;; pnm.scm -- Guile PNM format parser.  The main module.

;; Copyright (C) 2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains procedures to read data in PNM format to scheme
;; representation and vice versa.


;;; Code:

(define-module (pnm)
  #:use-module (oop goops)
  #:use-module (pnm image)
  #:use-module (pnm fsm p1)
  #:use-module (pnm fsm p1-context)
  #:use-module (pnm fsm p2)
  #:use-module (pnm fsm p2-context)
  #:use-module (pnm fsm p3)
  #:use-module (pnm fsm p3-context)
  #:use-module (pnm fsm p4)
  #:use-module (pnm fsm p4-context)
  #:use-module (pnm fsm p5)
  #:use-module (pnm fsm p5-context)
  #:use-module (pnm fsm p6)
  #:use-module (pnm fsm p6-context)
  #:use-module (pnm fsm pnm)
  #:use-module (pnm fsm pnm-context)
  #:use-module (pnm fsm context)
  #:use-module ((pnm fsm u8-context) #:renamer (symbol-prefix-proc 'u8:))
  #:use-module (pnm core error)
  #:export (pnm-type
            pnm->scm
            scm->pnm

            ;; Specific format handlers
            pbm-ascii->scm
            pbm-binary->scm
            pgm-ascii->scm
            pgm-binary->scm
            ppm-ascii->scm
            ppm-binary->scm))

(define* (pnm-type #:optional
                   (port (current-input-port))
                   #:key
                   (debug-mode? #f))
  "Read data from a @var{port} and determine if the data is a PNM image.  Return
the type of the image as a symbol, or #f if the input data is not a PNM image."
  (let* ((fsm     (make <pnm-fsm> #:debug-mode? debug-mode?))
         (context (make-char-context #:port port)))
    (catch 'pnm-error
      (lambda ()
        (let ((new-context (fsm-run! fsm context)))
          (context-result new-context)))
      (lambda (key . args)
        #f))))



(define* (pbm-ascii->scm #:optional
                         (port (current-input-port))
                         #:key
                         (debug-mode? #f))
  "Read and parse PBM ASCII data (P1) from a @var{port}, return a
@code{<pbm-ascii-image>} instance, or throw a @code{pnm-error} on errors."
  (let* ((fsm         (make <p1-fsm> #:debug-mode? debug-mode?))
         (context     (make-char-context #:port port))
         (new-context (fsm-run! fsm context))
         (result      (context-result new-context)))
    (make <pbm-ascii-image>
      #:commentary (assoc-ref result 'comment)
      #:width      (assoc-ref result 'width)
      #:height     (assoc-ref result 'height)
      #:data       (assoc-ref result 'data))))

(define* (pbm-binary->scm #:optional
                          (port (current-input-port))
                          #:key
                          (debug-mode? #f))
  "Read and parse PBM binary data (P4) from a @var{port}, return a
@code{<pbm-binary-image>} instance, or throw a @code{pnm-error} on errors."
  (let* ((fsm         (make <p4-fsm> #:debug-mode? debug-mode?))
         (context     (u8:make-u8-context #:port port))
         (new-context (fsm-run! fsm context))
         (result      (u8:context-result new-context)))
    (make <pbm-binary-image>
      #:commentary (assoc-ref result 'comment)
      #:width      (assoc-ref result 'width)
      #:height     (assoc-ref result 'height)
      #:data       (assoc-ref result 'data))))


;; PGM

(define* (pgm-ascii->scm #:optional
                         (port (current-input-port))
                         #:key
                         (debug-mode? #f))
  "Read and parse PGM ASCII (P2) data from a @var{port}, return a
@code{<pgm-image>} instance, or throw a @code{pnm-error} on errors."
  (let* ((fsm         (make <p2-fsm> #:debug-mode? debug-mode?))
         (context     (make-char-context #:port port))
         (new-context (fsm-run! fsm context))
         (result      (context-result new-context)))
    (make <pgm-ascii-image>
      #:commentary (assoc-ref result 'comment)
      #:width      (assoc-ref result 'width)
      #:height     (assoc-ref result 'height)
      #:grayscale-maxiumum-value (assoc-ref result 'grayscale)
      #:data       (assoc-ref result 'data))))

(define* (pgm-binary->scm #:optional
                          (port (current-input-port))
                          #:key
                          (debug-mode? #f))
  "Read and parse PGM binary (P5) data from a @var{port}, return a
@code{<pgm-binary-image>} instance, or throw a @code{pnm-error} on errors."
  (let* ((fsm         (make <p5-fsm> #:debug-mode? debug-mode?))
         (context     (u8:make-u8-context #:port port))
         (new-context (fsm-run! fsm context))
         (result      (u8:context-result new-context)))
    (make <pgm-binary-image>
      #:commentary (assoc-ref result 'comment)
      #:width      (assoc-ref result 'width)
      #:height     (assoc-ref result 'height)
      #:grayscale-maxiumum-value (assoc-ref result 'grayscale)
      #:data       (assoc-ref result 'data))))


;; PPM

(define* (ppm-ascii->scm #:optional
                         (port (current-input-port))
                         #:key
                         (debug-mode? #f))
  "Read and parse PPM ASCII (P3) data from a @var{port}, return a
@code{<ppm-image>} instance, or throw a @code{pnm-error} on errors."
  (let* ((fsm         (make <p3-fsm> #:debug-mode? debug-mode?))
         (context     (make-char-context #:port port))
         (new-context (fsm-run! fsm context))
         (result      (context-result new-context)))
    (make <ppm-ascii-image>
      #:commentary (assoc-ref result 'comment)
      #:width      (assoc-ref result 'width)
      #:height     (assoc-ref result 'height)
      #:color-maxiumum-value (assoc-ref result 'color)
      #:data       (assoc-ref result 'data))))

(define* (ppm-binary->scm #:optional
                          (port (current-input-port))
                          #:key
                          (debug-mode? #f))
  "Read and parse PPM binary (P6) data from a @var{port}, return a
@code{<ppm-binary-image>} instance, or throw a @code{pnm-error} on errors."
  (let* ((fsm         (make <p6-fsm> #:debug-mode? debug-mode?))
         (context     (u8:make-u8-context #:port port))
         (new-context (fsm-run! fsm context))
         (result      (u8:context-result new-context)))
    (make <ppm-binary-image>
      #:commentary (assoc-ref result 'comment)
      #:width      (assoc-ref result 'width)
      #:height     (assoc-ref result 'height)
      #:color-maxiumum-value (assoc-ref result 'color)
      #:data       (assoc-ref result 'data))))


;; Generic PNM handler.

(define* (pnm->scm #:optional
                   (port (current-input-port))
                   #:key
                   (debug-mode? #f))
  "Read and parse PNM image data from a @var{port}, return an
instance of a @code{<pnm-image>} sub-class, or throw a @code{pnm-error} on
errors."
  (let ((type (pnm-type port)))
    (case type
      ((pbm-ascii)
       (pbm-ascii->scm port #:debug-mode? debug-mode?))
      ((pgm-ascii)
       (pgm-ascii->scm port #:debug-mode? debug-mode?))
      ((ppm-ascii)
       (ppm-ascii->scm port #:debug-mode? debug-mode?))
      ((pbm-binary)
       (pbm-binary->scm port #:debug-mode? debug-mode?))
      ((pgm-binary)
       (pgm-binary->scm port #:debug-mode? debug-mode?))
      ((ppm-binary)
       (ppm-binary->scm port #:debug-mode? debug-mode?))
      (else
       (pnm-error "Unsupported image format" type)))))

(define* (scm->pnm image
                   #:optional
                   (port (current-output-port)))
  "Print a PNM @var{image} to a @var{port}."
  (pnm-image->pnm image port))

;; pnm.scm ends here.
