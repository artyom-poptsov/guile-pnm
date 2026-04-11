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
  #:use-module (pnm fsm pbm)
  #:use-module (pnm fsm pgm)
  #:use-module (pnm fsm context)
  #:use-module (pnm fsm pbm-context)
  #:use-module (pnm fsm pgm-context)
  #:export (pbm->scm
            pgm->scm
            scm->pbm
            scm->pgm))



(define* (pbm->scm #:optional
                   (port (current-input-port))
                   #:key
                   (debug-mode? #f))
  (let* ((fsm         (make <pbm-fsm> #:debug-mode? debug-mode?))
         (context     (make-char-context #:port port))
         (new-context (fsm-run! fsm context))
         (result      (context-result new-context)))
    (make <pbm-image>
      #:commentary (assoc-ref result 'comment)
      #:width      (assoc-ref result 'width)
      #:height     (assoc-ref result 'height)
      #:data       (assoc-ref result 'data))))

(define* (scm->pbm image
                   #:optional
                   (port (current-output-port)))
  (pnm-image->pnm image port))


;; PGM

(define* (pgm->scm #:optional
                   (port (current-input-port))
                   #:key
                   (debug-mode? #f))
  (let* ((fsm         (make <pgm-fsm> #:debug-mode? debug-mode?))
         (context     (make-char-context #:port port))
         (new-context (fsm-run! fsm context))
         (result      (context-result new-context)))
    (make <pgm-image>
      #:commentary (assoc-ref result 'comment)
      #:width      (assoc-ref result 'width)
      #:height     (assoc-ref result 'height)
      #:grayscale-maxiumum-value (assoc-ref result 'grayscale)
      #:data       (assoc-ref result 'data))))

(define* (scm->pgm image
                   #:optional
                   (port (current-output-port)))
  (pnm-image->pnm image port))

;; pnm.scm ends here.
