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

(ert-deftest test-google-translate--insert-translation-title/source-auto/detected ()
  (should
   (string-equal
    "Translate from English (detected) to Russian:\n"
    (with-temp-buffer
      (google-translate--insert-translation-title "auto" "ru" "en" "Translate from %s to %s:\n")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-google-translate--insert-translation-title/source-auto/detected-nil ()
  (should
   (string-equal
    "Translate from English to Russian:\n"
    (with-temp-buffer
      (google-translate--insert-translation-title "en" "ru" nil "Translate from %s to %s:\n")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-google-translate--insert-text-phonetic/do-not-show-phonetic ()
  (should
   (string-equal
    ""
    (with-temp-buffer
      (google-translate--insert-text-phonetic "phonetic" "%s")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-google-translate--insert-text-phonetic/show-phonetic-but-empty ()
  (setq google-translate-show-phonetic t)
  (should
   (string-equal
    ""
    (with-temp-buffer
      (google-translate--insert-text-phonetic "" "%s")
      (buffer-substring-no-properties (point-min) (point-max)))))
  (setq google-translate-show-phonetic nil))

(ert-deftest test-google-translate--insert-text-phonetic/show-phonetic ()
  (setq google-translate-show-phonetic t)
  (should
   (string-equal
    "phonetic"
    (with-temp-buffer
      (google-translate--insert-text-phonetic "phonetic" "%s")
      (buffer-substring-no-properties (point-min) (point-max)))))
  (setq google-translate-show-phonetic nil))

(ert-deftest test-google-translate--insert-translated-text ()
  (should
   (string-equal
    "translation"
    (with-temp-buffer
      (google-translate--insert-translated-text "translation" "%s")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-google-translate--insert-suggestion ()
  (should
   (string-equal
    "\nDid you mean: suggest\n"
    (with-temp-buffer
      (google-translate--insert-suggestion "suggest" "en" "ru")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-google-translate--suggestion-action ()
  (with-temp-buffer
    (let* ((suggestion "suggestion")
           (source-language "en")
           (target-language "ru")
           (button (insert-text-button "Foo"
                                       'action 'test
                                       'suggestion suggestion
                                       'source-language source-language
                                       'target-language target-language)))
      (with-mock
       (mock (google-translate-translate source-language
                                         target-language
                                         suggestion))
       (google-translate--suggestion-action button)))))

(ert-deftest test-google-translate--insert-translation-phonetic/do-not-show-phonetic ()
  (should
   (string-equal
    ""
    (with-temp-buffer
      (google-translate--insert-translation-phonetic "phonetic" "%s")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-google-translate--insert-translation-phonetic/show-phonetic-but-empty ()
  (setq google-translate-show-phonetic t)
  (should
   (string-equal
    ""
    (with-temp-buffer
      (google-translate--insert-translation-phonetic "" "%s")
      (buffer-substring-no-properties (point-min) (point-max)))))
  (setq google-translate-show-phonetic nil))

(ert-deftest test-google-translate--insert-translation-phonetic/show-phonetic ()
  (setq google-translate-show-phonetic t)
  (should
   (string-equal
    "phonetic"
    (with-temp-buffer
      (google-translate--insert-translation-phonetic "phonetic" "%s")
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
