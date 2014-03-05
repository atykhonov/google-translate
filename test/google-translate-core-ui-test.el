;; (ert-deftest test-google-translate-request-words-fixtures ()
;;   (dolist (file (f-files google-translate-test/word-fixture-path))
;;     (let ((fixture (th-google-translate-load-fixture file)))
;;       (th-google-translate-request-fixture fixture)
;;       ;; assertions are skipped. In case of no errors assume that test pass.
;; )))

(ert-deftest test-google-translate-language-abbreviation/English/en ()
  (should
   (string-equal
    (google-translate-language-abbreviation "English")
    "en")))

(ert-deftest test-google-translate-language-abbreviation/Detect-Language/auto ()
  (should
   (string-equal
    (google-translate-language-abbreviation "Detect language")
    "auto")))

(ert-deftest test-google-translate-language-display-name/auto/unspecified ()
  (should
   (string-equal
    (google-translate-language-display-name "auto")
    "unspecified language")))

(ert-deftest test-google-translate-language-display-name/en/English ()
  (should
   (string-equal
    (google-translate-language-display-name "en")
    "English")))

(ert-deftest test-google-translate--buffer-output-translation-title/source-auto/detected ()
  (should
   (string-equal
    "Translate from English (detected) to Russian:\n"
    (with-temp-buffer
      (google-translate--buffer-output-translation-title "auto" "ru" "en")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-google-translate--buffer-output-translation-title/source-auto/detected ()
  (should
   (string-equal
    "Translate from English to Russian:\n"
    (with-temp-buffer
      (google-translate--buffer-output-translation-title "en" "ru" nil)
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-google-translate--buffer-output-text-phonetic/do-not-show-phonetic ()
  (should
   (string-equal
    ""
    (with-temp-buffer
      (google-translate--buffer-output-text-phonetic "phonetic")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-google-translate--buffer-output-text-phonetic/show-phonetic-but-empty ()
  (setq google-translate-show-phonetic t)
  (should
   (string-equal
    ""
    (with-temp-buffer
      (google-translate--buffer-output-text-phonetic "")
      (buffer-substring-no-properties (point-min) (point-max)))))
  (setq google-translate-show-phonetic nil))

(ert-deftest test-google-translate--buffer-output-text-phonetic/show-phonetic ()
  (setq google-translate-show-phonetic t)
  (should
   (string-equal
    "\nphonetic\n"
    (with-temp-buffer
      (google-translate--buffer-output-text-phonetic "phonetic")
      (buffer-substring-no-properties (point-min) (point-max)))))
  (setq google-translate-show-phonetic nil))

(ert-deftest test-google-translate--buffer-output-translation ()
  (should
   (string-equal
    "\ntranslation\n"
    (with-temp-buffer
      (google-translate--buffer-output-translation "translation")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-google-translate--buffer-output-suggestion ()
  (should
   (string-equal
    "\nDid you mean: suggest\n"
    (with-temp-buffer
      (google-translate--buffer-output-suggestion "suggest")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-google-translate--buffer-output-translation-phonetic/do-not-show-phonetic ()
  (should
   (string-equal
    ""
    (with-temp-buffer
      (google-translate--buffer-output-translation-phonetic "phonetic")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-google-translate--buffer-output-translation-phonetic/show-phonetic-but-empty ()
  (setq google-translate-show-phonetic t)
  (should
   (string-equal
    ""
    (with-temp-buffer
      (google-translate--buffer-output-translation-phonetic "")
      (buffer-substring-no-properties (point-min) (point-max)))))
  (setq google-translate-show-phonetic nil))

(ert-deftest test-google-translate--buffer-output-translation-phonetic/show-phonetic ()
  (setq google-translate-show-phonetic t)
  (should
   (string-equal
    "\nphonetic\n"
    (with-temp-buffer
      (google-translate--buffer-output-translation-phonetic "phonetic")
      (buffer-substring-no-properties (point-min) (point-max)))))
  (setq google-translate-show-phonetic nil))

(ert-deftest test-google-translate-read-source-language/detect-language ()
  (with-mock
   (stub google-translate-completing-read => "Detect language")
   (should
    (string-equal
     (google-translate-read-source-language)
     "auto"))))

(ert-deftest test-google-translate-read-source-language/english ()
  (with-mock
   (stub google-translate-completing-read => "English")
   (should
    (string-equal
     (google-translate-read-source-language)
     "en"))))
