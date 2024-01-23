;;; cl-bytes.el --- CL operations with bytes in integers in Emacs Lisp  -*- lexical-binding: t -*-

;; Author: Dima Akater <nuclearspace@gmail.com>
;; Maintainer: Dima Akater <nuclearspace@gmail.com>
;; Version: 0.0.20240123.214908
;; Package-Requries: ((emacs "24.3"))

;; Copyright (C) 2023--2024  Dima Akater

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; [[file:cl-bytes.org::*Dependencies][Dependencies:1]]
(eval-when-compile (require 'cl-lib))
;; Dependencies:1 ends here

;; [[file:cl-bytes.org::*Definition][Definition:1]]
(cl-defstruct byte-specifier
  "Byte specifier."
  (size 0 :type (integer 0))
  (position 0 :type (integer 0)))
;; Definition:1 ends here

;; [[file:cl-bytes.org::*Definition][Definition:1]]
(defun cl-byte (size position)
  "Return a byte of width SIZE w/ weights 2^POSITION + SIZE - 1 through 2^POSITION.

Return a byte specifier that indicates a byte of width SIZE and
whose bits have weights 2^POSITION + SIZE - 1 through
2^POSITION."
  (make-byte-specifier :size size :position position))
;; Definition:1 ends here

;; [[file:cl-bytes.org::*Definition][Definition:1]]
(defalias 'cl-byte-size 'byte-specifier-size)
(defalias 'cl-byte-position 'byte-specifier-position)
;; Definition:1 ends here

;; [[file:cl-bytes.org::*Definition][Definition:1]]
(defun cl--%dpb (newbyte size posn integer)
  ;; (declare (type bit-index size posn) (explicit-check))
  (let ((mask (1- (ash 1 size))))
    (logior (logand integer (lognot (ash mask posn)))
            (ash (logand newbyte mask) posn))))

(defun cl-dpb (newbyte bytespec integer)
  "Return an integer that is the same as INTEGER except in the BYTESPEC bits.

Used to replace a field of bits within integer.

“dpb” stands for “deposit byte”.

Let s be the size specified by BYTESPEC; then the low s bits of
NEWBYTE appear in the result in the byte specified by
BYTESPEC. NEWBYTE is interpreted as being right-justified, as if
it were the result of `cl-ldb'."
  (cl--%dpb newbyte
            (cl-byte-size bytespec)
            (cl-byte-position bytespec)
            integer))

(defun cl--%ldb (size posn integer)
  ;; The naive algorithm is horrible in the general case.
  ;; Consider (LDB (BYTE 1 2) (SOME-GIANT-BIGNUM)) which has to shift the
  ;; input rightward 2 bits, consing a new bignum just to read 1 bit.
  (logand (ash integer (- posn))
          (1- (ash 1 size))))

(defun cl-ldb (bytespec integer)
  "Extract and return the byte of INTEGER specified by BYTESPEC.

Return an integer in which the bits with weights 2^(s-1) through
2^0 are the same as those in INTEGER with weights 2^(p+s-1)
through 2^p, and all other bits zero; s is (cl-byte-size
BYTESPEC) and p is (cl-byte-position BYTESPEC).

`setf' may be used with `cl-ldb' to modify a byte within the
INTEGER that is stored in a given place. The order of evaluation,
when a `cl-ldb' form is supplied to `setf', is exactly
left-to-right.  The effect is to perform a `cl-dpb' operation and
then store the result back into the place."
  (cl--%ldb (cl-byte-size bytespec)
            (cl-byte-position bytespec)
            integer))
;; Definition:1 ends here

;; [[file:cl-bytes.org::*Dependencies][Dependencies:1]]
(eval-when-compile (require 'gv)
                   (require 'macroexp))
;; Dependencies:1 ends here

;; [[file:cl-bytes.org::*setf][setf:1]]
(gv-define-expander cl-ldb
  (lambda (do bytespec place)
    (macroexp-let2 nil b bytespec
      (funcall do place
               (lambda (v)
                 (macroexp-let2 nil n v
                   `(progn (setf ,place (cl-dpb ,n ,b ,place)) ,n)))))))
;; setf:1 ends here

(provide 'cl-bytes)

;;; cl-bytes.el ends here
