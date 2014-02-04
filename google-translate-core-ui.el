;;; google-translate-core-ui.el --- google translate core UI

;; Copyright (C) 2012 Oleksandr Manzyuk <manzyuk@gmail.com>

;; Author: Oleksandr Manzyuk <manzyuk@gmail.com>
;; URL: https://github.com/atykhonov/google-translate
;; Version: 0.7.0
;; Keywords: convenience

;; Contributors:
;;   Tassilo Horn <tsdh@gnu.org>
;;   Bernard Hurley <bernard@marcade.biz>
;;   Chris Bilson <cbilson@pobox.com>
;;   Andrey Tykhonov <atykhonov@gmail.com>

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

;; This script provides the most common functions and variables for
;; UI. It does not contain any interactive functions and overall is
;; not going to be used directly by means of
;; `execute-extended-command' (M-x). Its purpose to provide the most
;; valuable and useful functionality for packages and scripts which
;; provide UI.
;;
;; The most important functions are the following:
;;
;; - `google-translate-translate'
;;
;; - `google-translate-read-source-language'
;;
;; - `google-translate-read-target-language'
;;
;; `google-translate-translate' queries the source and target
;; languages and text to translate, and shows a buffer with available
;; translations of the text. `google-translate-read-source-language'
;; reads source language from minibuffer and
;; `google-translate-read-target-language' reads target language from
;; minibuffer.
;; 
;; Customization:

;; You can customize the following variables:
;;
;; - `google-translate-enable-ido-completion'
;;
;; - `google-translate-show-phonetic'
;;
;; If `google-translate-enable-ido-completion' is non-NIL, the input
;; will be read with ido-style completion.
;;
;; The variable `google-translate-show-phonetic' controls whether the
;; phonetic spelling of the original text and its translation is
;; displayed if available.  If you want to see the phonetics, set this
;; variable to t.
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
;;
;;
;;; Code:
;;

(require 'google-translate-core)
(require 'ido)


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

(defgroup google-translate-core-ui nil
  "Emacs core UI script for the Google Translate package."
  :group 'processes)

(defcustom google-translate-enable-ido-completion nil
  "If non-NIL, use `ido-completing-read' rather than
  `completing-read' for reading input."
  :group 'google-translate-core-ui
  :type  '(choice (const :tag "No"  nil)
                  (other :tag "Yes" t)))

(defcustom google-translate-show-phonetic nil
  "If non-NIL, try to show the phonetic spelling."
  :group 'google-translate-core-ui
  :type '(choice (const :tag "No"  nil)
                 (const :tag "Yes" t)))

(defface google-translate-text-face
  '((t (:inherit default)))
  "Face used to display the original text."
  :group 'google-translate-core-ui)

(defface google-translate-phonetic-face
  '((t (:inherit shadow)))
  "Face used to display the phonetic spelling."
  :group 'google-translate-core-ui)

(defface google-translate-translation-face
  '((t (:weight bold)))
  "Face used to display the probable translation."
  :group 'google-translate-core-ui)


(defun google-translate-supported-languages ()
  "Return a list of names of languages supported by Google Translate."
  (mapcar #'car google-translate-supported-languages-alist))

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

(defun google-translate-paragraph (text face)
  "Insert TEXT as a filled paragraph into the current buffer and
apply FACE to it."
  (let ((beg (point)))
    (insert (format "\n%s\n" text))
    (facemenu-set-face face beg (point))
    (fill-region beg (point))))

(defun google-translate--buffer-output-translation-title (source-language 
                                                          target-language 
                                                          auto-detected-language)
  "Outputs in buffer translation title which contains
information about used while translating source and target
languages."
  (insert (format "Translate from %s to %s:\n"
                  (if (string-equal source-language "auto")
                      (format "%s (detected)"
                              (google-translate-language-display-name
                               auto-detected-language))
                    (google-translate-language-display-name
                     source-language))
                  (google-translate-language-display-name
                   target-language))))

(defun google-translate--buffer-output-translating-text (text)
  "Outputs in buffer translating text."
  (google-translate-paragraph
   text
   'google-translate-text-face))

(defun google-translate--buffer-output-text-phonetic (text-phonetic)
  "Outputs in buffer TEXT-PHONETIC in case of
`google-translate-show-phonetic' is set to t."
  (when (and google-translate-show-phonetic
             (not (string-equal text-phonetic "")))
    (google-translate-paragraph
     text-phonetic
     'google-translate-phonetic-face)))

(defun google-translate--buffer-output-translation (translation)
  "Output in buffer TRANSLATION."
  (google-translate-paragraph
   translation
   'google-translate-translation-face))

(defun google-translate--buffer-output-translation-phonetic (translation-phonetic)
  "Output in buffer TRANSLATION-PHONETIC in case of
`google-translate-show-phonetic' is set to t."
  (when (and google-translate-show-phonetic
             (not (string-equal translation-phonetic "")))
    (google-translate-paragraph
     translation-phonetic
     'google-translate-phonetic-face)))

(defun google-translate--buffer-output-detailed-translation (detailed-translation translation)
  "Output in buffer DETAILED-TRANSLATION for the given TRANSLATION."
  (loop for item across detailed-translation do
        (let ((index 0))
          (unless (string-equal (aref item 0) "")
            (insert (format "\n%s\n" (aref item 0)))
            (loop for translation across (aref item 1) do
                  (insert (format "%2d. %s\n"
                                  (incf index) translation)))))))

(defun google-translate-translate (source-language target-language text)
  "Translate TEXT from SOURCE-LANGUAGE to TARGET-LANGUAGE.

Pops up a buffer named *Google Translate* with available translations
of TEXT. To deal with multi-line regions, sequences of white space
are replaced with a single space. If the region contains not text, a
message is printed."
  (let* ((buffer-name "*Google Translate*")
         (json (google-translate-request source-language
                                         target-language
                                         text)))
    (if (null json)
        (message "Nothing to translate.")
      (let (
            (auto-detected-language (aref json 2))
            (text-phonetic (google-translate-json-text-phonetic json))
            (translation (google-translate-json-translation json))
            (translation-phonetic (google-translate-json-translation-phonetic json))
            (detailed-translation (google-translate-json-detailed-translation json)))

        (with-output-to-temp-buffer buffer-name
          (set-buffer buffer-name)
          (google-translate--buffer-output-translation-title source-language
                                                             target-language
                                                             auto-detected-language)
          (google-translate--buffer-output-translating-text text)
          (google-translate--buffer-output-text-phonetic text-phonetic)
          (google-translate--buffer-output-translation translation)
          (google-translate--buffer-output-translation-phonetic translation-phonetic)
          (when detailed-translation
            (google-translate--buffer-output-detailed-translation
             detailed-translation
             translation)))))))

(defun google-translate-read-source-language (&optional prompt)
  "Read a source language, with completion, and return its abbreviation.

The null input is equivalent to \"Detect language\"."
  (let ((completion-ignore-case t)
        (prompt
         (if (null prompt)
             "Translate from: "
           prompt)))
    (google-translate-language-abbreviation
     (google-translate-completing-read
      prompt
      (google-translate-supported-languages)
      "Detect language"))))

(defun google-translate-read-target-language (&optional prompt)
  "Read a target language, with completion, and return its abbreviation.

The input is guaranteed to be non-null."
  (let ((completion-ignore-case t)
        (prompt
         (if (null prompt)
             "Translate to: "
           prompt)))
    (cl-flet ((read-language ()
                             (google-translate-completing-read
                              prompt
                              (google-translate-supported-languages))))
      (let ((target-language (read-language)))
        (while (string-equal target-language "")
          (setq target-language (read-language)))
        (google-translate-language-abbreviation target-language)))))

(defun google-translate-completing-read (prompt choices &optional def)
  "Read a string in the minibuffer with completion.

If `google-translate-enable-ido-completion' is non-NIL, use
ido-style completion."
  (funcall (if google-translate-enable-ido-completion
               #'ido-completing-read
             #'completing-read)
           prompt choices nil t nil nil def))


(provide 'google-translate-core-ui)

;;; google-translate-core-ui.el ends here
