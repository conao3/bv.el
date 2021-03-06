;;; bv.el --- Bool-vector utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1"))
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

;; Bool-vector utilities.


;;; Code:

(require 'cl-lib)

(defgroup bv nil
  "Bool-vector utilities."
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/conao3/bv.el"))

(defun bv-make (length &optional init)
  "Return a new `bool-vector' of LENGTH, using INIT for each element."
  (make-bool-vector (* (ceiling length 4) 4) init))

(defalias 'bv-p 'bool-vector-p)
(defalias 'bv-ior 'bool-vector-union)
(defalias 'bv-xor 'bool-vector-exclusive-or)
(defalias 'bv-and 'bool-vector-intersection)
(defalias 'bv-diff 'bool-vector-set-difference)
(defalias 'bv-not 'bool-vector-not)
(defalias 'bv-subsetp 'bool-vector-subsetp)
(defalias 'bv-count-consecutive 'bool-vector-count-consecutive)
(defalias 'bv-count-population 'bool-vector-count-population)
(defalias 'bv-get 'aref)
(defalias 'bv-set 'aset)

(defun bv--from-base (str pow)
  "Bool-vector from STR based 2^POW."
  (let* ((base (ash 1 pow))
         (nibbles (mapcar
                   (lambda (elm) (string-to-number (char-to-string elm) base))
                   (nreverse (string-to-list str))))
         (bv (bv-make (* pow (length nibbles)))))
    (let ((i 0))
      (dolist (n nibbles)
        (dotimes (_ pow)
          (aset bv i (> (logand 1 n) 0))
          (cl-incf i)
          (setf n (ash n -1)))))
    bv))

(defun bv--to-base (a base)
  "Return hex string from A based BASE."
  (let ((n 1)
        (res 0))
    (dotimes (i (length a))
      (when (aref a i)
        (cl-incf res n))
      (setq n (lsh n 1)))
    (with-temp-buffer
      (let (d)
        (while (< 0 res)
          (setq d (mod res base))
          (if (< d 10)
              (insert (number-to-string d))
            (insert (char-to-string (+ ?a (- d 10)))))
          (setq res (/ res base))))
      (reverse (buffer-string)))))

(defun bv-from-hex (str)
  "Bool-vector from hex STR.

Example:
  (bv-from-hex \"0\")
  ;;=> #&4\" \"

  (bv-from-hex \"7\")
  ;;=> #&4\"\"

  (bv-from-hex \"f\")
  ;;=> #&4\"\"

  (bv-from-hex \"21\")
  ;;=> #&8\"!\""
  (bv--from-base str 4))

(defun bv-from-oct (str)
  "Bool-vector from oct STR."
  (bv--from-base str 2))

(defun bv-from-bin (str)
  "Bool-vector from bin STR."
  (bv--from-base str 1))

(defun bv-from-dec (int)
  "Bool-vector from INT."
  (bv-from-hex (format "%x" int)))

(defun bv-to-hex (a)
  "Return hex string from A.

Example:
  (bv-from-hex \"0\")
  ;;=> \"0\"

  (bv-from-hex \"f\")
  ;;=> \"f\"

  (bv-from-hex \"21\")
  ;;=> \"21\""
  (bv--to-base a 4))

(defun bv-to-oct (a)
  "Return obt string from A."
  (bv--to-base a 2))

(defun bv-to-bin (a)
  "Return obt string from A."
  (bv--to-base a 1))

(defun bv-to-list (a)
  "Course A to `list'.

Example:
  (bv-to-list (bv-from-hex \"0\"))
  ;;=> nil

  (bv-to-list (bv-from-hex \"3\"))
  ;;=> (1 2)

  (bv-to-list (bv-from-hex \"7\"))
  ;;=> (1 2 4)

  (bv-to-list (bv-from-hex \"f\"))
  ;;=> (1 2 4 8)"
  (let ((n 1) res)
    (dotimes (i (length a))
      (when (aref a i)
        (push n res))
      (setq n (lsh n 1)))
    (nreverse res)))

(defun bv-mapc (fn a)
  "`mapc' on A appling FN."
  (mapc fn (bv-to-list a)))

(defun bv-mapcar (fn a)
  "`mapcar' on A appling FN."
  (mapcar fn (bv-to-list a)))

(defmacro bv-dolist (spec &rest body)
  "`dolist' on vb.

SPEC is list like (var vb).
BODY is dolist body."
  `(dolist (,(car spec) (bv-to-list ,(cdr spec)))
     ,@body))

(defun bv-nclear (a &optional from to)
  "Clear `bool-vector' A destructively.
If specify FROM or TO, clear between theirs."
  (cl-loop for i from (or from 0) below (or to (length a))
           do (aset a i nil)
           return a))

(defun bv-clear (a &optional from to)
  "Clear `bool-vector' A.
If specify FROM or TO, clear between theirs."
  (bv-nclear (bv-clone a) from to))

(defun bv-nflip (a &optional from to)
  "Flip A bit FROM TO destructively."
  (cl-loop for i from (or from 0) below (or to (length a))
           do (aset a i (not (aref a i)))
           return a))

(defun bv-flip (a &optional from to)
  "Flip A bit FROM TO."
  (bv-nflip (bv-clone a) from to))

(defun bv-nshift (a count)
  "Left shift A amount of COUNT destructively."
  (let ((len (length a)))
    (cond
     ((= 0 count) a)
     ((< 0 count)
      (let ((bv (bv-make (* (ceiling (+ len count) 4) 4))))
        (cl-loop for i from 0 below len
                 do (aset bv (+ i count) (aref a i))
                 return bv)))
     (t
      (cl-loop for i from 0 below (- len count)
               do (aset a i (aref a (+ i count))))
      (cl-loop for i from (- len count) below len
               do (aset a i nil)
               return a)))))

(defun bv-cardinality (a)
  "Return count of t in A."
  (let ((n 1)
        (res 0))
    (dotimes (i (length a))
      (when (aref a i)
        (cl-incf res))
      (setq n (lsh n 1)))
    res))

(defun bv-emptyp (a)
  "Return t if A is empty."
  (= 0 (bv-cardinality a)))

(defun bv-anyp (a)
  "Return t if A has any t element."
  (not (bv-emptyp a)))

(defun bv-allp (a)
  "Return t if A has all t element."
  (= (length a) (bv-cardinality a)))

(defun bv-clone (a)
  "Return clone of A."
  (let ((bv (bv-make (length a))))
    (setq bv (bv-ior a bv))))

(defun bv-test (a int)
  "Return if A has INT element."
  (bv-anyp (bv-and a (bv--from-base int 10))))

(defun bv-same-length (a b)
  "Return t if A and B are same length."
  (= (length a) (length b)))

(defun bv-equals (a b)
  "Return t if A equals B."
  (catch 'break
    (unless (bv-same-length a b)
      (throw 'break nil))
    (dotimes (i (length a))
      (unless (eq (aref a i) (aref b i))
        (throw 'break nil)))
    t))

(defun bv--int-to-bin (int &optional len)
  "Translate INT into binary notation in LEN with 0 padding."
  (let ((i 0)
        (res (make-string (or len (* (ceiling (sqrt (+ int 1)) 4) 4)) ?0)))
    (while (< 0 int)
      (when (< 0 (logand int 1))
        (aset res i ?1))
      (setq int (/ int 2))
      (cl-incf i))
    (reverse res)))

(defun bv-show-table ()
  "Show ASCII tbale with `bool-vector' Elisp notation."
  (interactive)
  (with-help-window (get-buffer-create "*bv ASCII Table*")
    (princ "| Dec | Hex |    Bin    |    bv    || Dec | Hex |    Bin    |    bv    |\n")
    (princ "|-----+-----+-----------+----------++-----+-----+-----------+----------|\n")
    (cl-loop for i from 0 to 63
             for j = (+ i 64)
             do (princ (format "| %03d | %03x |  %s | %8s || %03d | %03x |  %s | %8s |\n"
                                i i (bv--int-to-bin i 8) (bv-from-dec i)
                                j j (bv--int-to-bin j 8) (bv-from-dec j))))))

(provide 'bv)

;;; bv.el ends here
