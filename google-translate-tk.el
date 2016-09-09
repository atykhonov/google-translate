;;; google-translate-tk.el --- functions for generation `tk'
;;; parameter.

;; Copyright (C) 2012 Oleksandr Manzyuk <manzyuk@gmail.com>

;; Author: Oleksandr Manzyuk <manzyuk@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/google-translate
;; Version: 0.11.13
;; Keywords: convenience

;; Contributors:
;;   Tassilo Horn <tsdh@gnu.org>
;;   Bernard Hurley <bernard@marcade.biz>
;;   Chris Bilson <cbilson@pobox.com>
;;   Takumi Kinjo <takumi.kinjo@gmail.com>
;;   momomo5717 <momomo5717@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains functions for generation `tk' parameter which is
;; required to make a valid request to the Google Translate.

;;; Code:

(require 'cl-lib)


(defvar google-translate--bit-v-len 32)

(defvar google-translate--tkk-url
  "http://translate.google.com/")

(defvar google-translate--tkk-regex
  "TKK=eval('((function(){var\\s-+a\\\\x3d\\(-?[0-9]+\\);var\\s-+b\\\\x3d\\(-?[0-9]+\\);return\\s-+\\([0-9]+\\)"
  "Regexp for `google-translate--search-tkk'.")

(defvar google-translate--tkk-debug
  nil
  "For debugging of tk related issues.")

(defun google-translate--bit-v-2comp (v)
  "Return the two's complement of V."
  (let* ((vc (vconcat v))
         (len (length vc)))
    ;; Complement of v
    (cl-loop for i from 0 below len do
             (aset vc i (logxor (aref vc i) 1)))
    ;; vc = complement of v + 1
    (cl-loop for i downfrom (1- len) to 0
             do (aset vc i (logxor (aref vc i) 1))
             when (> (aref vc i) 0) return nil)
    vc))

(defun google-translate--number-to-bit-v (n)
  "Return a bit vector from N."
  (if (< n 0) (google-translate--bit-v-2comp
               (google-translate--number-to-bit-v (abs n)))
    (let ((v (make-vector google-translate--bit-v-len 0)))
      (cl-loop for i downfrom (1- google-translate--bit-v-len) to 0
               with q
               when (< n 1) return nil do
               (setq q (ffloor (* n 0.5)))
               (aset v i (floor (- n (* 2.0 q))))
               (setq n q))
      v)))

(defun google-translate--bit-v-to-number (v)
  "Return a floating-point number from V."
  (if (and (> (aref v 0) 0)
           ;; Exclude [1 0 ... 0]
           (cl-loop for i from 1 below google-translate--bit-v-len
                    thereis (> (aref v i) 0)))
      (- (google-translate--bit-v-to-number (google-translate--bit-v-2comp v)))
    (funcall (if (> (aref v 0) 0)  #'- #'+)
             (cl-reduce (lambda (acc e) (+ (* acc 2.0) e))
                        v :initial-value 0.0))))

(defun google-translate--logfn (fn n1 n2)
  "Helper function for logical FN."
  (let ((v1 (google-translate--number-to-bit-v n1))
        (v2 (google-translate--number-to-bit-v n2))
        (v (make-vector google-translate--bit-v-len 0)))
    (cl-loop for i from 0 below google-translate--bit-v-len do
             (aset v i (funcall fn (aref v1 i) (aref v2 i))))
    (google-translate--bit-v-to-number v)))

(defun google-translate--logand (n1 n2)
  "Return a floating-point number from N1 and N2."
  (google-translate--logfn #'logand n1 n2))

(defun google-translate--logxor (n1 n2)
  "Return a floating-point number from N1 and N2."
  (google-translate--logfn #'logxor n1 n2))

(defun google-translate--lsh (n d)
  "Return a floating-point number.
Shift the bits in N to the left or rihgt D places.
D is an integer."
  (let ((v (google-translate--number-to-bit-v n))
        (v-result (make-vector google-translate--bit-v-len 0)))
    (if (< d 0) ;; Shift Right Logical
        ;; [x0 x1 ... xn-d ... xn] => [0 ... 0 x0 x1 ... xn-d]
        (cl-loop for i from (abs d) below google-translate--bit-v-len
                 for j from 0 do
                 (aset v-result i (aref v j)))
      ;; Shift Left Logical
      ;; [x0 x1 ... xd ... xn] => [xd ... xn 0 ... 0]
      (cl-loop for i from d below google-translate--bit-v-len
               for j from 0 do
               (aset v-result j (aref v i))))
    (google-translate--bit-v-to-number v-result)))

(defun google-translate--search-tkk ()
  "Search TKK."
  (if (re-search-forward google-translate--tkk-regex nil t)
      (mapcar #'string-to-number
              (list (match-string 1) (match-string 2) (match-string 3)))
    (error "Failed to search TKK")))

(defun google-translate--get-b-d1 ()
  "Return a list of b and d1 for `google-translate--gen-tk'."
  (let* ((url-request-extra-headers '(("Connection" . "close")))
         (buf (url-retrieve-synchronously google-translate--tkk-url))
         (debug-buffer-name "*Google Translate Debug*")
         tkk-contents
         tkk-ls)
    (with-current-buffer buf
      (setq tkk-ls (google-translate--search-tkk))
      (setq tkk-contents (buffer-string)))
    (when google-translate--tkk-debug
      (with-output-to-temp-buffer debug-buffer-name
        (prin1 tkk-contents))
      (select-window (display-buffer debug-buffer-name)))
    (when (buffer-live-p buf) (kill-buffer buf))
    (list (cl-third tkk-ls)
          (google-translate--lsh (+ (cl-first tkk-ls) (cl-second tkk-ls)) 0))))

(defun google-translate--gen-rl (a b)
  (cl-loop for c from 0 below (- (length b) 2) by 3
           for d = (aref b (+ c 2)) do
           (setq d (if (>= d ?a) (- d 87) (- d ?0)))
           (setq d (if (= (aref b (1+ c)) ?+)
                       (google-translate--lsh a (- d))
                     (google-translate--lsh a d)))
           (setq a (if (= (aref b c) ?+)
                       (google-translate--logand (+ a d) 4294967295.0)
                     (google-translate--logxor a d))))
  a)

(defun google-translate--gen-tk (text &optional b-d1)
  (setq b-d1 (or b-d1 (google-translate--get-b-d1)))
  (let* ((b (cl-first b-d1))
         (d1 (cl-second b-d1))
         (ub "+-3^+b+-f")
         (vb "+-a^+6")
         (a (cl-reduce (lambda (a e) (google-translate--gen-rl (+ a e) vb))
                       (encode-coding-string text 'utf-8) :initial-value b)))
    (setq a (google-translate--gen-rl a ub))
    (setq a (google-translate--logxor a d1))
    (when (< a 0) ;; (abs a) + 2^31
      (setq a (+ (google-translate--logand a 2147483647.0) 2147483648.0)))
    (setq a (ffloor (mod a 1e6)))
    (format "%s.%s"
            (car (split-string (number-to-string a) "\\."))
            (car (split-string (number-to-string (google-translate--logxor a b)) "\\.")))))


(provide 'google-translate-tk)
;;; google-translate-tk.el ends here
