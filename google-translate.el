;;; google-translate.el --- Emacs interface to Google Translate

;; Copyright (C) 2012 Oleksandr Manzyuk <manzyuk@gmail.com>

;; Author: Oleksandr Manzyuk <manzyuk@gmail.com>
;; Version: 0.4
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

(require 'ert)
(require 'ido)
(require 'json)
(require 'url)

(defvar google-translate-supported-languages-alist
  '(("Afrikaans"           . "af")
    ("Albanian"            . "sq")
    ("Arabic"              . "ar")
    ("Armenian"            . "hy")
    ("Azerbaijani"         . "az")
    ("Basque"              . "eu")
    ("Belarusian"          . "be")
    ("Bengali"             . "bn")
    ("Bulgarian"           . "bg")
    ("Chinese Simplified"  . "zh-CN")
    ("Chinese Traditional" . "zh-TW")
    ("Croatian"            . "hr")
    ("Czech"               . "cs")
    ("Danish"              . "da")
    ("Dutch"               . "nl")
    ("English"             . "en")
    ("Estonian"            . "et")
    ("Filipino"            . "tl")
    ("Finnish"             . "fi")
    ("French"              . "fr")
    ("Galician"            . "gl")
    ("Georgian"            . "ka")
    ("German"              . "de")
    ("Greek"               . "el")
    ("Gujarati"            . "gu")
    ("Haitian Creole"      . "ht")
    ("Hebrew"              . "iw")
    ("Hindi"               . "hi")
    ("Hungarian"           . "hu")
    ("Icelandic"           . "is")
    ("Indonesian"          . "id")
    ("Irish"               . "ga")
    ("Italian"             . "it")
    ("Japanese"            . "ja")
    ("Kannada"             . "kn")
    ("Korean"              . "ko")
    ("Latin"               . "la")
    ("Latvian"             . "lv")
    ("Lithuanian"          . "lt")
    ("Macedonian"          . "mk")
    ("Malay"               . "ms")
    ("Maltese"             . "mt")
    ("Norwegian"           . "no")
    ("Persian"             . "fa")
    ("Polish"              . "pl")
    ("Portuguese"          . "pt")
    ("Romanian"            . "ro")
    ("Russian"             . "ru")
    ("Serbian"             . "sr")
    ("Slovak"              . "sk")
    ("Slovenian"           . "sl")
    ("Spanish"             . "es")
    ("Swahili"             . "sw")
    ("Swedish"             . "sv")
    ("Tamil"               . "ta")
    ("Telugu"              . "te")
    ("Thai"                . "th")
    ("Turkish"             . "tr")
    ("Ukrainian"           . "uk")
    ("Urdu"                . "ur")
    ("Vietnamese"          . "vi")
    ("Welsh"               . "cy")
    ("Yiddish"             . "yi"))
  "Alist of the languages supported by Google Translate.

Each element is a cons-cell of the form (NAME . CODE), where NAME
is a human-readable language name and CODE is its code used as a
query parameter in HTTP requests.")

(defgroup google-translate nil
  "Emacs interface to Google Translate."
  :group 'processes)

(defcustom google-translate-default-source-language nil
  "Default source language.

A string designating a language supported by Google Translate.
Set this variable to NIL (the default value) if you want to
always be queried for the source language, or to \"auto\" if you
want Google Translate to always detect the source language.

See the variable `google-translate-supported-languages-alist' for
the list of available languages."
  :group 'google-translate
  :type  `(radio ,@(mapcar #'(lambda (lang)
                               `(const :tag ,(car lang) ,(cdr lang)))
                           google-translate-supported-languages-alist)
                 (const :tag "Detect language" "auto")
                 (other :tag "Always ask" nil)))

(defcustom google-translate-default-target-language nil
  "Default target language.

A string designating a language supported by Google Translate.
Set this variable to NIL (the default value) if you want to
always be queried for the target language.

See the variable `google-translate-supported-languages-alist' for
the list of available languages."
  :group 'google-translate
  :type  `(radio ,@(mapcar #'(lambda (lang)
                               `(const :tag ,(car lang) ,(cdr lang)))
                           google-translate-supported-languages-alist)
                 (other :tag "Always ask" nil)))

(defcustom google-translate-enable-ido-completion nil
  "If non-NIL, use `ido-completing-read' rather than
  `completing-read' for reading input."
  :group 'google-translate
  :type  '(choice (const :tag "No"  nil)
                  (other :tag "Yes" t)))

(defcustom google-translate-show-phonetic nil
  "If non-NIL, try to show the phonetic spelling."
  :group 'google-translate
  :type '(choice (const :tag "No"  nil)
                 (const :tag "Yes" t)))

(defface google-translate-text-face
  '((t (:inherit default)))
  "Face used to display the original text."
  :group 'google-translate)

(defface google-translate-phonetic-face
  '((t (:inherit shadow)))
  "Face used to display the phonetic spelling."
  :group 'google-translate)

(defface google-translate-translation-face
  '((t (:weight bold)))
  "Face used to display the probable translation."
  :group 'googel-translate)

;; `ido-completing-read', unlike `completing-read', expects a list of
;; strings (`completing-read' is more flexible and accepts an alist).
(defun google-translate-supported-languages ()
  "Return a list of names of languages supported by Google Translate."
  (mapcar #'car google-translate-supported-languages-alist))

(defun google-translate-completing-read (prompt choices &optional def)
  "Read a string in the minibuffer with completion.

If `google-translate-enable-ido-completion' is non-NIL, use
ido-style completion."
  (funcall (if google-translate-enable-ido-completion
               #'ido-completing-read
             #'completing-read)
           prompt choices nil t nil nil def))

(defvar google-translate-base-url
  "http://translate.google.com/translate_a/t")

(defun google-translate-format-query-string (query-params)
  "Format QUERY-PARAMS as a query string.

QUERY-PARAMS must be an alist of field-value pairs."
  (mapconcat #'(lambda (p)
                 (format "%s=%s"
                         (url-hexify-string (car p))
                         (url-hexify-string (cdr p))))
             query-params "&"))

(defun google-translate-format-request-url (query-params)
  "Format QUERY-PARAMS as a Google Translate HTTP request URL.

QUERY-PARAMS must be an alist of field-value pairs."
  (concat google-translate-base-url
          "?"
          (google-translate-format-query-string query-params)))

(defun google-translate-http-response-body (url)
  "Retrieve URL and return the response body as a string."
  (with-current-buffer (url-retrieve-synchronously url)
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (re-search-forward (format "\n\n"))
    (delete-region (point-min) (point))
    (prog1 (buffer-string) (kill-buffer))))

;; Google Translate responses with an almost valid JSON string
;; respresentation except that the nulls appear to be dropped.
;; In particular the response may contain the substrings "[,",
;; ",,", and ",]".  `google-translate-insert-nulls' undoes
;; that.
(defun google-translate-insert-nulls (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "\\(\\[,\\|,,\\|,\\]\\)" (point-max) t)
      (backward-char)
      (insert "null"))
    (buffer-string)))

(ert-deftest test-insert-nulls ()
  (should (string-equal
           (google-translate-insert-nulls "[,[,[,,],,],,]")
           "[null,[null,[null,null,null],null,null],null,null]")))

(defun google-translate-paragraph (text face)
  "Insert TEXT as a filled paragraph into the current buffer and
apply FACE to it."
  (let ((beg (point)))
    (insert (format "\n%s\n" text))
    (facemenu-set-face face beg (point))
    (fill-region beg (point))))

(defun google-translate-translate (source-language target-language text)
  "Translate TEXT from SOURCE-LANGUAGE to TARGET-LANGUAGE.

Pops up a buffer named *Google Translate* with available translations
of TEXT.  To deal with multi-line regions, sequences of white space
are replaced with a single space.  If the region contains not text, a
message is printed."
  (let ((text-stripped
         (replace-regexp-in-string "[[:space:]\n\r]+" " " text)))
    (if (or (= (length text-stripped) 0)
            (string= text-stripped " "))
        (message "Nothing to translate.")
      (let* ((json
              (json-read-from-string
               (google-translate-insert-nulls
                (google-translate-http-response-body
                 (google-translate-format-request-url
                  `(("client" . "t")
                    ("ie"     . "UTF-8")
                    ("oe"     . "UTF-8")
                    ("sl"     . ,source-language)
                    ("tl"     . ,target-language)
                    ("text"   . ,text-stripped)))))))
             (text-phonetic
              (mapconcat #'(lambda (item) (aref item 3))
                         (aref json 0) ""))
             (translation
              (mapconcat #'(lambda (item) (aref item 0))
                         (aref json 0) ""))
             (translation-phonetic
              (mapconcat #'(lambda (item) (aref item 2))
                         (aref json 0) ""))
             (dict (aref json 1)))
        (with-output-to-temp-buffer "*Google Translate*"
          (set-buffer "*Google Translate*")
          (insert
           (format "Translate from %s to %s:\n"
                   (if (string-equal source-language "auto")
                       (format "%s (detected)"
                               (google-translate-language-display-name
                                (aref json 2)))
                     (google-translate-language-display-name
                      source-language))
                   (google-translate-language-display-name
                    target-language)))
          (google-translate-paragraph
           text
           'google-translate-text-face)
          (when (and google-translate-show-phonetic
                     (not (string-equal text-phonetic "")))
            (google-translate-paragraph
             text-phonetic
             'google-translate-phonetic-face))
          (google-translate-paragraph
           translation
           'google-translate-translation-face)
          (when (and google-translate-show-phonetic
                     (not (string-equal translation-phonetic "")))
            (google-translate-paragraph
             translation-phonetic
             'google-translate-phonetic-face))
          (when dict
            ;; DICT is, if non-nil, a dictionary article represented by
            ;; a vector of items, where each item is a 2-element vector
            ;; whose zeroth element is the name of a part of speech and
            ;; whose first element is a vector of translations for that
            ;; part of speech.
            (loop for item across dict do
                  (let ((index 0))
                    (unless (string-equal (aref item 0) "")
                      (insert (format "\n%s\n" (aref item 0)))
                      (loop for translation across (aref item 1) do
                            (insert (format "%2d. %s\n"
                                            (incf index) translation))))))))))))

(defun google-translate-read-source-language (prompt)
  "Read a source language, with completion, and return its abbreviation.

The null input is equivalent to \"Detect language\"."
  (let ((completion-ignore-case t))
    (google-translate-language-abbreviation
     (google-translate-completing-read
      prompt
      (google-translate-supported-languages)
      "Detect language"))))

(defun google-translate-read-target-language (prompt)
  "Read a target language, with completion, and return its abbreviation.

The input is guaranteed to be non-null."
  (let ((completion-ignore-case t))
    (cl-flet ((read-language ()
               (google-translate-completing-read
                prompt
                (google-translate-supported-languages))))
      (let ((target-language (read-language)))
        (while (string-equal target-language "")
          (setq target-language (read-language)))
        (google-translate-language-abbreviation target-language)))))

(defun google-translate-language-abbreviation (language)
  "Return the abbreviation of LANGUAGE."
  (if (string-equal language "Detect language")
      "auto"
    (cdr (assoc language google-translate-supported-languages-alist))))

(defun google-translate-language-display-name (abbreviation)
  "Return a name suitable for use in prompts of the language whose
abbreviation is ABBREVIATION."
  (if (string-equal abbreviation "auto")
      "unspecified language"
    (car (rassoc abbreviation google-translate-supported-languages-alist))))

(defun google-translate-read-args (override-p reverse-p)
  "Query and return the language arguments of `google-translate-translate'.

When OVERRIDE-P is NIL, the source (resp. target) language is queried
only if the variable `google-translate-default-source-language' (resp.
`google-translate-default-target-language') is NIL.  If OVERRIDE-P is
non-NIL, both the source and target languages are queried, allowing
one to override the defaults if they are specified.

REVERSE-P is used to reverse the default direction of translation: if
it's non-NIL, the value of `google-translate-default-source-language'
becomes the default target language and vice versa."
  (let* ((default-source-language
           (if reverse-p
               google-translate-default-target-language
             google-translate-default-source-language))
         (default-target-language
           (if reverse-p
               google-translate-default-source-language
             google-translate-default-target-language))
         (source-language
          (if (and default-source-language
                   (not override-p))
              default-source-language
            (google-translate-read-source-language
             "Translate from: ")))
         (target-language
          (if (and default-target-language
                   (not override-p))
              default-target-language
            (google-translate-read-target-language
             (format "Translate from %s to: "
                     (google-translate-language-display-name
                      source-language))))))
    (list source-language target-language)))

(defun %google-translate-query-translate (override-p reverse-p)
  (let* ((langs (google-translate-read-args override-p reverse-p))
         (source-language (car langs))
         (target-language (cadr langs)))
    (google-translate-translate source-language target-language
     (read-from-minibuffer
      (format "Translate from %s to %s: "
              (google-translate-language-display-name source-language)
              (google-translate-language-display-name target-language))))))

(defun google-translate-query-translate (&optional override-p)
  "Interactively translate text with Google Translate.

Query a text (a word or a phrase), and pop up a buffer named *Google
Translate* displaying available translations of the text.

If no defaults for the source and target languages are specified (by
setting the variables `google-translate-default-source-language' and
`google-translate-default-target-language'), interactively query the
missing parts.  For example, a reasonable option may be to specify a
default for the target language and always be queried for the source
language.

With a `C-u' prefix argument, query the source and target languages,
even if any defaults are specified.  For example, you may frequently
need to translate from English to Russian, and you may choose to set
the default source and target languages to \"en\" and  \"ru\", resp.
However, occasionally you may also need to translate from Russian to
English.  With a `C-u' prefix argument you can override the defaults
and specify the source and target languages explicitly.

The languages are queried with completion, and the null input at the
source language prompt is considered as an instruction for Google
Translate to detect the source language."
  (interactive "P")
  (%google-translate-query-translate override-p nil))

(defun google-translate-query-translate-reverse (&optional override-p)
  "Like `google-translate-query-translate', but performs translation
in the reverse direction.

The value of the variable `google-translate-default-source-language'
\(if set) becomes the target language, and the value of the variable
`google-translate-default-target-language' (if also set) becomes the
source language.

In particular, when both variables are set, translation is performed
in the reverse direction."
  (interactive "P")
  (%google-translate-query-translate override-p t))

(defun %google-translate-at-point (override-p reverse-p)
  (let* ((langs (google-translate-read-args override-p reverse-p))
         (source-language (car langs))
         (target-language (cadr langs)))
    (google-translate-translate
     source-language target-language
     (if (use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end))
       (or (current-word t)
           (error "No word at point."))))))

(defun google-translate-at-point (&optional override-p)
  "Translate the word at point or the words in the active region.

For the meaning of OVERRIDE-P, see `google-translate-query-translate'."
  (interactive "P")
  (%google-translate-at-point override-p nil))

(defun google-translate-at-point-reverse (&optional override-p)
  "Like `google-translate-at-point', but performs translation in the
reverse direction."
  (interactive "P")
  (%google-translate-at-point override-p t))

(provide 'google-translate)

;;; google-translate.el ends here
