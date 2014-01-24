(require 'google-translate)

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


(defgroup google-translate-base-ui nil
  "Emacs base UI interface to Google Translate Core."
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


(require 'ido)


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
               (google-translate--insert-nulls
                (google-translate--http-response-body
                 (google-translate--format-request-url
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

(provide 'google-translate-core-ui)

;;; google-translate-core-ui.el ends here
