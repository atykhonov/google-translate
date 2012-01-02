(require 'url)
(require 'json)

(defvar google-translate-supported-languages
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

Each element is a cons-cell of the form (NAME . ABBREVIATION), where
NAME is a human-readable language name and ABBREVIATION is its
internal name used as a query parameter in HTTP requests.")

(defgroup google-translate nil
  "Emacs interface to Google Translate."
  :group 'processes)

(defcustom google-translate-default-source-language nil
  "Default source language.

A string designating a language supported by Google Translate.
Set this variable to NIL (the default value) if you want to
always be queried for the source language, or to \"auto\" if you
want Google Translate to always detect the source language.

See the variable `google-translate-supported-languages' for the
list of available languages."
  :group 'google-translate
  :type  `(radio ,@(mapcar #'(lambda (lang)
			       `(const :tag ,(car lang) ,(cdr lang)))
			   google-translate-supported-languages)
		 (const :tag "Detect language" "auto")
		 (other :tag "Always ask" nil)))

(defcustom google-translate-default-target-language nil
  "Default target language.

A string designating a language supported by Google Translate.
Set this variable to NIL (the default value) if you want to
always be queried for the target language.

See the variable `google-translate-supported-languages' for the
list of available languages."
  :group 'google-translate
  :type  `(radio ,@(mapcar #'(lambda (lang)
			       `(const :tag ,(car lang) ,(cdr lang)))
			   google-translate-supported-languages)
		 (other :tag "Always ask" nil)))

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
  (let ((buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer buffer)
      (set-buffer-multibyte t)
      (goto-char (point-min))
      (re-search-forward (format "\n\n"))
      (kill-region (point-min) (point))
      (prog1 (buffer-string) (kill-buffer buffer)))))

;; Google Translate responses with an almost valid JSON string
;; respresentation except that the nulls appear to be dropped.
;; In particular, the response may contain the substrings ",,"
;; and ",]".  `google-translate-insert-nulls' undoes that.
(defun google-translate-insert-nulls (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "," (point-max) t)
	(when (or (looking-at ",")
		  (looking-at "]"))
	  (insert "null")))
    (buffer-string)))

(defun google-translate-translate (source-language target-language text)
  "Translate TEXT from SOURCE-LANGUAGE to TARGET-LANGUAGE.

Pops up a buffer named *Google Translate* with available translations
of TEXT."
  (let* ((json
	  (json-read-from-string
	   (google-translate-insert-nulls
	    ;; Google Translate won't let us make a request unless we
	    ;; send a "User-Agent" header it recognizes.
	    ;; "Mozilla/5.0" seems to work.
	    (let ((url-request-extra-headers
		   '(("User-Agent" . "Mozilla/5.0"))))
	      (google-translate-http-response-body
	       (google-translate-format-request-url
		`(("client" . "t")
		  ("sl"     . ,source-language)
		  ("tl"     . ,target-language)
		  ("text"   . ,text))))))))
	 (dict (aref json 1)))
    (with-output-to-temp-buffer "*Google Translate*"
      (set-buffer "*Google Translate*")
      (insert text)
      (facemenu-set-face 'bold (point-min) (point-max))
      (insert "\n")
      (goto-char (point-min))
      (insert
       (format "Translate from %s to %s:\n\n"
	       (if (string-equal source-language "auto")
		   (format "%s (detected)"
			   (google-translate-language-display-name
			    (aref json 2)))
		 (google-translate-language-display-name
		  source-language))
	       (google-translate-language-display-name
		target-language)))
      (goto-char (point-max))
      (if dict
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
					  (incf index) translation))))))
	;; If DICT is nil, which occurs, for example, if we ask for a
	;; translation of a whole phrase, then show the most probable
	;; translation, found as the leftmost string in the JSON.
	(insert (format "\n%s\n" (aref (aref (aref json 0) 0) 0)))))))

(defun google-translate-read-source-language (prompt)
  "Read a source language, with completion, and return its abbreviation.

The null input is equivalent to \"Detect language\"."
  (let ((completion-ignore-case t))
    (google-translate-language-abbreviation
     (completing-read
      prompt
      google-translate-supported-languages
      nil t nil nil "Detect language"))))

(defun google-translate-read-target-language (prompt)
  "Read a target language, with completion, and return its abbreviation.

The input is guaranteed to be non-null."
  (let ((completion-ignore-case t))
    (flet ((read-language ()
	     (completing-read
	      prompt
	      google-translate-supported-languages
	      nil t)))
      (let ((target-language (read-language)))
	(while (string-equal target-language "")
	  (setq target-language (read-language)))
	(google-translate-language-abbreviation target-language)))))

(defun google-translate-language-abbreviation (language)
  "Return the abbreviation of LANGUAGE."
  (if (string-equal language "Detect language")
      "auto"
    (cdr (assoc language google-translate-supported-languages))))

(defun google-translate-language-display-name (abbreviation)
  "Return a name suitable for use in prompts of the language whose
abbreviation is ABBREVIATION."
  (if (string-equal abbreviation "auto")
      "unspecified language"
    (car (find-if #'(lambda (lang)
		      (string-equal abbreviation (cdr lang)))
		  google-translate-supported-languages))))

(defun google-translate-read-args (override-p)
  "Query and return the arguments of `google-translate-translate'.

When OVERRIDE-P is NIL, the source (resp. target) language is queried
only if the variable `google-translate-default-source-language' (resp.
`google-translate-default-target-language') is NIL.  If OVERRIDE-P is
non-NIL, both the source and target languages are queried, allowing
one to override the defaults if they are specified."
  (let* ((source-language
	  (if (and google-translate-default-source-language
		   (not override-p))
	      google-translate-default-source-language
	    (google-translate-read-source-language
	     "Translate from: ")))
	 (target-language
	  (if (and google-translate-default-target-language
		   (not override-p))
	      google-translate-default-target-language
	    (google-translate-read-target-language
	     (format "Translate from %s to: "
		     (google-translate-language-display-name
		      source-language)))))
	 (text
	  (read-from-minibuffer
	   (format "Translate from %s to %s: "
		   (google-translate-language-display-name
		    source-language)
		   (google-translate-language-display-name
		    target-language)))))
    (list source-language target-language text)))

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
  (apply #'google-translate-translate
	 (google-translate-read-args override-p)))

(provide 'google-translate)
