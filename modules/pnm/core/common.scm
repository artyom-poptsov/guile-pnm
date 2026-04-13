;;; common.scm -- Guile PNM common procedures.

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

;; This module contains common procedures for Guile-PNM.


;;; Code:

(define-module (pnm core common)
  #:use-module (oop goops)
  #:export (constructor-argument
            object-address/hex-string))


(define (object-address/hex-string object)
  (number->string (object-address object) 16))


(define (constructor-argument keyword initargs)
  (and (memq keyword initargs)
       (cadr (memq keyword initargs))))

;; common.scm ends here.
