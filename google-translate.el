;;; google-translate.el --- Emacs interface to Google Translate

;; Copyright (C) 2012 Oleksandr Manzyuk <manzyuk@gmail.com>

;; Author: Oleksandr Manzyuk <manzyuk@gmail.com>
;; Version: 0.5
;; Keywords: convenience

;; Contributors:
;;   Tassilo Horn <tsdh@gnu.org>
;;   Bernard Hurley <bernard@marcade.biz>
;;   Chris Bilson <cbilson@pobox.com>

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

;; Invoking the function `google-translate-query-translate' queries the source
;; and target languages and text to translate, and shows a buffer with
;; available translations of the text.  Invoking the function
;; `google-translate-at-point' translates the word at point or the active
;; region.

;; Installation:

;; Assuming that the file `google-translate.el' is somewhere on the
;; load path, add the following lines to your .emacs file:
;;
;;   (require 'google-translate)
;;   (global-set-key "\C-ct" 'google-translate-at-point)
;;   (global-set-key "\C-cT" 'google-translate-query-translate)
;;
;; Change the key bindings to your liking.


;; Customization:

;; You can customize the following variables:
;;
;; - `google-translate-enable-ido-completion'
;;
;; - `google-translate-default-source-language'
;;
;; - `google-translate-default-target-language'

;; If `google-translate-enable-ido-completion' is non-NIL, the input
;; will be read with ido-style completion.

;; If the variable `google-translate-default-source-language' is set
;; to a non-NIL value, the source language won't be queried and that
;; value will be used instead.  Analogously, if you set the variable
;; `google-translate-default-target-language' to some non-NIL value,
;; that value will be used without querying.

;; You can always override this behavior by supplying a `C-u' prefix
;; argument to the function `google-translate-query-translate'.

;; Here is an example.  Suppose that your native language is Russian
;; and you frequently need to translate from various languages to
;; Russian.  Then it is reasonable
;;
;; - to set the variable `google-translate-default-target-language'
;;   to "ru", and
;;
;; - to leave `google-translate-default-source-language' set to its
;;   default value, NIL.
;;
;; In this case, the function `google-translate-query-translate' is
;; only going to query the source language and text to translate.
;; If you need to translate to some language other than Russian, you
;; can override the default for the target language by supplying a
;; `C-u' prefix argument, in which case you will be queried for both
;; the source and target languages, as well as text to translate.

;; If you frequently translate from some fixed language, it is also
;; reasonable to set `google-translate-default-source-language' to
;; an appropriate value.
;;
;; If you have both the default source and target languages specified,
;; you may like to bind functions `google-translate-at-point-reverse'
;; and `google-translate-query-translate-reverse' to some keys, e.g.:
;;
;;   (global-set-key (kbd "C-c r") 'google-translate-at-point-reverse)
;;   (global-set-key (kbd "C-c R") 'google-translate-query-translate-reverse)
;;
;; This will allow you to quickly translate in the reverse direction.
;; When the default source (resp. target) language is not set, the
;; target (resp. source) language of the reverse translation will be
;; queried interactively.

;; The admitted values of `google-translate-default-source-language'
;; and `google-translate-default-target-language' are the codes of the
;; languages supported by Google Translate (like "ru" for Russian
;; above).  See `google-translate-supported-languages' for the list of
;; the supported languages, or customize the defaults using the
;; customization mechanism of Emacs.  Setting a default language to
;; NIL means that language will always be queried.  Moreover, the
;; variable `google-translate-default-source-language' can be set to a
;; special value "auto" that is interpreted as the instruction for
;; Google Translate to detect the source language.  This option is
;; also available when you are queried for the source language: simply
;; leave this parameter blank by pressing RET.  (If you have enabled
;; the ido-style completion, "Detect language" is going to be the
;; first option, which you can select simply by hitting RET.)
;;
;; The variable `google-translate-show-phonetic' controls whether the
;; phonetic spelling of the original text and its translation is
;; displayed if available.  If you want to see the phonetics, set this
;; variable to T.
;;
;; There are also three faces you can customize:
;;
;; - `google-translate-text-face', used to display the original text
;;   (defaults to `default')
;;
;; - `google-translate-phonetic-face', used to display the phonetics
;;   (defaults to `shadow')
;;
;; - `google-translate-translation-face', used to display the highest
;;   ranking translation (defaults to `default' with the `weight'
;;   attribute set to `bold')
;;
;; For example, to show the translation in a larger font change the
;; `height' attribute of the face `google-translate-translation-face'
;; like so:
;;
;;   (set-face-attribute 'google-translate-translation-face nil :height 1.4)


;;; Code:

(eval-when-compile (require 'cl))

(require 'json)
(require 'url)

(defgroup google-translate nil
  "Emacs interface to Google Translate."
  :group 'processes)

(defvar google-translate-base-url
  "http://translate.google.com/translate_a/t")

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

(defun google-translate--http-response-body (url)
  "Retrieve URL and return the response body as a string."
  (with-current-buffer (url-retrieve-synchronously url)
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (re-search-forward (format "\n\n"))
    (delete-region (point-min) (point))
    (prog1 (buffer-string) (kill-buffer))))

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
  (json-read-from-string
   (google-translate--insert-nulls
    (google-translate--http-response-body
     (google-translate--format-request-url
      `(("client" . "t")
        ("ie"     . "UTF-8")
        ("oe"     . "UTF-8")
        ("sl"     . ,source-language)
        ("tl"     . ,target-language)
        ("text"   . ,text)))))))

(defun google-translate-json-text-phonetic (json)
  "Retrieve from the JSON (which returns by the
`google-translate-request' function) phonetic transcription of
the translating text."
  (mapconcat (lambda (item) (aref item 3))
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
  (mapconcat #'(lambda (item) (aref item 2))
             (aref json 0) ""))

(defun google-translate-json-detailed-translation (json)
  "Retrieve from the JSON (which returns by the
`google-translate-request' function) a dictionary article
represented by a vector of items, where each item is a 2-element
vector whose zeroth element is the name of a part of speech and
whose first element is a vector of translations for that part of
speech."
  (aref json 1))

(provide 'google-translate)

;;; google-translate.el ends here
