;;; google-translate-core.el --- google-translate core script.

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

;; This file is the core of `google-translate' package. It does contains
;; the most important and vital functions and variables for the
;; package functionality.
;;
;; The most important is the `google-translate-request' function which
;; is intended to be used by other scripts and packages, expecially by
;; the packages which provides UI. 
;;
;; This, `google-translate-core', package doesn't provide any UI.
;;
;; `google-translate-request' function sends http request to the
;; google service and returns JSON response which contains translation
;; and other info.
;;
;; There are also a set of helper functions which are going to be
;; useful to retrieve data from the mentioned JSON response:
;;
;; `google-translate-json-text-phonetic' - retrieves text phonetic; 
;;
;; `google-translate-json-translation' - retrieves translation;
;;
;; `google-translate-json-translation-phonetic' - retrieves
;; phonetic translation;
;;
;; `google-translate-json-detailed-translation' - retrieves
;; additional, detailed information which relates to the
;; translation.
;;

;;; Code:

(eval-when-compile (require 'cl))

(require 'json)
(require 'url)
(require 'google-translate-tk)

(defgroup google-translate-core nil
  "Google Translate core script."
  :group 'processes)

(defvar google-translate-base-url
  "http://translate.google.com/translate_a/single")

(defvar google-translate-listen-url
  "http://translate.google.com/translate_tts")

(defun google-translate--format-query-string (query-params)
  "Format QUERY-PARAMS as a query string.

QUERY-PARAMS must be an alist of field-value pairs."
  (mapconcat #'(lambda (p)
                 (format "%s=%s"
                         (url-hexify-string (car p))
                         (url-hexify-string (cdr p))))
             query-params "&"))

(defun google-translate--format-request-url (query-params)
  "Format QUERY-PARAMS as a Google Translate HTTP request URL.

QUERY-PARAMS must be an alist of field-value pairs."
  (concat google-translate-base-url
          "?"
          (google-translate--format-query-string query-params)))

(defun google-translate--format-listen-url (query-params)
  "Format QUERY-PARAMS as a Google Translate HTTP request URL for listen translation.

QUERY-PARAMS must be an alist of field-value pairs."
  (concat google-translate-listen-url
          "?"
          (google-translate--format-query-string query-params)))

(defun google-translate-format-listen-url (text language)
  "Format listen url for TEXT and TARGET-LANGUAGE."
  (google-translate--format-listen-url `(("ie"      . "UTF-8")
                                         ("q"       . ,text)
                                         ("tl"      . ,language)
                                         ("total"   . "1")
                                         ("idx"     . "0")
                                         ("textlen" . ,(number-to-string (length text)))
                                         ("client"  . "t")
                                         ("prev"    . "input")
                                         ("tk"      . ,(google-translate--gen-tk text)))))

(defun google-translate--http-response-body (url &optional for-test-purposes)
  "Retrieve URL and return the response body as a string."
  (with-current-buffer (url-retrieve-synchronously url)
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (when (null for-test-purposes)
      (re-search-forward (format "\n\n"))
      (delete-region (point-min) (point)))
    (if (null for-test-purposes)
        (prog1 
            (buffer-string)
          (kill-buffer))
      (buffer-name))))

(defun google-translate--insert-nulls (string)
  "Google Translate responses with an almost valid JSON string
respresentation except that the nulls appear to be dropped. In
particular the response may contain the substrings \"[,\",
\",,\", and \",]\". This function undoes that."
  (with-temp-buffer
    (set-buffer-multibyte t)
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "\\(\\[,\\|,,\\|,\\]\\)" (point-max) t)
      (backward-char)
      (insert "null"))
    (buffer-string)))

(defun google-translate--trim-string (string)
  "Remove whitespaces in beginning and ending of STRING.
  White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n\r]*" ""
                            (replace-regexp-in-string "[ \t\n\r]*\\'" "" string)))

(defun google-translate--strip-string (string)
  "Replace spaces, tabs, line feeds (ASCII 10) and carridge
returns (ASCII 13) by a single space symbol."
  (replace-regexp-in-string "[[:space:]\n\r]+" " " string))

(defun google-translate-prepare-text-for-request (text)
  "Make TEXT as clean as possible berofe sending it in the
request."
  (google-translate--trim-string
   (google-translate--strip-string text)))

(defun google-translate-request (source-language target-language text)
  "Send to the Google Translate http request which consigned to
translate TEXT from SOURCE-LANGUAGE to TARGET-LANGUAGE. Returns
response in json format."
  (let ((cleaned-text (google-translate-prepare-text-for-request text)))
    (when (and
           (stringp cleaned-text)
           (> (length cleaned-text) 0))
      (json-read-from-string
       (google-translate--insert-nulls
        (google-translate--request source-language target-language text))))))

(defun google-translate--request (source-language
                                  target-language
                                  text
                                  &optional for-test-purposes)
  "Send to the Google Translate http request which consigned to
translate TEXT from SOURCE-LANGUAGE to TARGET-LANGUAGE."
  (google-translate--http-response-body
   (google-translate--format-request-url
    `(("client" . "t")
      ("ie"     . "UTF-8")
      ("oe"     . "UTF-8")
      ("sl"     . ,source-language)
      ("tl"     . ,target-language)
      ("q"      . ,text)
      ("dt"     . "bd")
      ("dt"     . "ex")
      ("dt"     . "ld")
      ("dt"     . "md")
      ("dt"     . "qc")
      ("dt"     . "rw")
      ("dt"     . "rm")
      ("dt"     . "ss")
      ("dt"     . "t")
      ("dt"     . "at")
      ("pc"     . "1")
      ("otf"    . "1")
      ("srcrom" . "1")
      ("ssel"   . "0")
      ("tsel"   . "0")
      ("tk"     . ,(google-translate--gen-tk text))))
   for-test-purposes))

(defun google-translate-json-text-phonetic (json)
  "Retrieve from the JSON (which returns by the
`google-translate-request' function) phonetic transcription of
the translating text."
  (mapconcat (lambda (item) (if (> (length item) 3) (aref item 3) ""))
             (aref json 0) ""))

(defun google-translate-json-translation (json)
  "Retrieve from the JSON (which returns by the
`google-translate-request' function) translation of the
translating text."
  (mapconcat #'(lambda (item) (aref item 0))
             (aref json 0) ""))

(defun google-translate-json-translation-phonetic (json)
  "Retrieve from the JSON (which returns by the
`google-translate-request' function) phonetic transcription of
the translating text."
  (mapconcat #'(lambda (item) (if (> (length item) 2) (aref item 2) ""))
             (aref json 0) ""))

(defun google-translate-json-detailed-translation (json)
  "Retrieve from the JSON (which returns by the
`google-translate-request' function) a dictionary article
represented by a vector of items, where each item is a 2-element
vector whose zeroth element is the name of a part of speech and
whose first element is a vector of translations for that part of
speech."
  (aref json 1))

(defun google-translate-json-detailed-definition (json)
  "Retrieve the definition of translating text in source language from the JSON
which returned by the `google-translate-request' function.

This function returns the definition if it's included within the JSON as 12th
element, or returns nil if not included.

The definition is a dictionary article represented by a vector of items, where
each item is a 2-element vector whose zeroth element is the name of a part of
speech and whose first element is a vector of definitions for that part of
speech."
  (if (> (length json) 12)
    (aref json 12)))

(defun google-translate-json-suggestion (json)
  "Retrieve from JSON (which returns by the
`google-translate-request' function) suggestion. This function
does matter when translating misspelled word. So instead of
translation it is possible to get suggestion."
  (let ((info (aref json 7)))
    (when info
      (aref info 1))))

(defun google-translate-version ()
  (interactive)
  (message "Google Translate (version): %s" "0.11.13"))


(provide 'google-translate-core)

;;; google-translate-core.el ends here
