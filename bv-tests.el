;;; bv-tests.el --- Test definitions for bv  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; URL: https://github.com/conao3/bv.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Test definitions for `bv'.


;;; Code:

(require 'cort)
(require 'bv)

(defun bv--hex (desc)
  "From Emacs data-tests.el from DESC."
  (let (bv nibbles)
    (dolist (c (string-to-list desc))
      (push (string-to-number (char-to-string c) 16) nibbles))
    (setf bv (make-bool-vector (* 4 (length nibbles)) nil))
    (let ((i 0))
      (dolist (n nibbles)
        (dotimes (_ 4)
          (aset bv i (> (logand 1 n) 0))
          (cl-incf i)
          (setf n (ash n -1)))))
    bv))

(defun bv--bin (desc)
  "From Emacs data-tests.el from DESC."
  (let (bv nibbles)
    (dolist (c (string-to-list desc))
      (push (string-to-number (char-to-string c) 2) nibbles))
    (setf bv (make-bool-vector (length nibbles) nil))
    (let ((i 0))
      (dolist (n nibbles)
        (dotimes (_ 1)
          (aset bv i (> (logand 1 n) 0))
          (cl-incf i)
          (setf n (ash n -1)))))
    bv))

(cort-deftest-generate bv-p :equal
  '(((bv-p (make-bool-vector 4 nil)) t)
    ((bv-p (make-list 4 nil)) nil)))

(cort-deftest-generate bv-ior :bv-equals
  '(((bv-ior (bv--bin "00000000") (bv--bin "01010101")) (bv--bin "01010101"))
    ((bv-ior (bv--bin "01111111") (bv--bin "01010101")) (bv--bin "01111111"))
    ((bv-ior (bv--bin "01111111") (bv--bin "01111111")) (bv--bin "01111111"))))

(cort-deftest-generate bv-xor :bv-equals
  '(((bv-xor (bv--bin "00000000") (bv--bin "01010101")) (bv--bin "01010101"))
    ((bv-xor (bv--bin "01111111") (bv--bin "01010101")) (bv--bin "00101010"))
    ((bv-xor (bv--bin "01111111") (bv--bin "01111111")) (bv--bin "00000000"))))

(cort-deftest-generate bv-and :bv-equals
  '(((bv-and (bv--bin "00000000") (bv--bin "01010101")) (bv--bin "00000000"))
    ((bv-and (bv--bin "01111111") (bv--bin "01010101")) (bv--bin "01010101"))
    ((bv-and (bv--bin "01111111") (bv--bin "01111111")) (bv--bin "01111111"))))

(cort-deftest-generate bv-diff :bv-equals
  '(((bv-diff (bv--bin "00000000") (bv--bin "01010101")) (bv--bin "00000000"))
    ((bv-diff (bv--bin "01111111") (bv--bin "01010101")) (bv--bin "00101010"))
    ((bv-diff (bv--bin "01111111") (bv--bin "01111111")) (bv--bin "00000000"))))

(cort-deftest-generate bv-not :bv-equals
  '(((bv-not (bv--bin "00000000")) (bv--bin "11111111"))
    ((bv-not (bv--bin "01010101")) (bv--bin "10101010"))
    ((bv-not (bv--bin "01111111")) (bv--bin "10000000"))))

(cort-deftest-generate bv-subsetp :eq
  '(((bv-subsetp (bv--bin "00000000") (bv--bin "11111111")) t)
    ((bv-subsetp (bv--bin "00001010") (bv--bin "00001110")) t)
    ((bv-subsetp (bv--bin "00001010") (bv--bin "00001010")) t)
    ((bv-subsetp (bv--bin "00001010") (bv--bin "00000010")) nil)
    ((bv-subsetp (bv--bin "00001010") (bv--bin "00000000")) nil)))

;; (provide 'bv-tests)

;;; bv-tests.el ends here
