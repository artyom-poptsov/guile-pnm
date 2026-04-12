;; guix.scm --- GNU Guix package recipe    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This file is part of Guile-PNM.
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
;;
;; GNU Guix development package. To use as the basis for a development
;; environment, run:
;;
;;  guix shell -D -f guix.scm
;;
;; In the new shell, run:
;;
;;  autoreconf -vif && ./configure && make check
;;
;;; Code:


(use-modules (guix gexp)
             (guix packages)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages bash)
             (gnu packages man)
             (gnu packages pkg-config)
             (gnu packages tex)
             (gnu packages texinfo))


(define %source-dir (dirname (current-filename)))


(define guile-pnm
  (package
    (name "guile-pnm")
    (version "git")
    (source (local-file %source-dir
                        #:recursive? #t
                        #:select? (git-predicate %source-dir)))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           pkg-config
           texinfo
           texlive-scheme-small
           help2man
           ;; needed when cross-compiling.
           guile-3.0
           guile-lib
           guile-smc))
    (inputs
     (list bash-minimal
           guile-3.0
           guile-lib))
    (propagated-inputs
     (list guile-smc))
    (arguments
     (list #:make-flags #~(list "GUILE_AUTO_COMPILE=0"))) ;to prevent guild warnings
    (home-page "https://github.com/artyom-poptsov/guile-pnm")
    (synopsis "Guile library for PNM (PBM, PGM, PPM) format support")
    (description
     "@code{guile-pnm} is a GNU Guile library for working with the
@url{https://en.wikipedia.org/wiki/Netpbm, PNM format}, including portable
bitmap (PBM), portable graymap (PGM) and portable pixel (PPM) variants.")
    (license gpl3)))

guile-pnm

;;; guix.scm ends here.
