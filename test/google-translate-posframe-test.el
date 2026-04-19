;;; google-translate-posframe-test.el --- Tests for posframe UI -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for google-translate-posframe-ui.el
;; Tests cover: cache, text detection, text truncation, and formatting.

;;; Code:

(require 'ert)
(require 'google-translate-posframe)

;;; Cache tests

(ert-deftest test-google-translate-posframe--cache-put-and-get ()
  "Test that cached translations can be stored and retrieved."
  (let ((google-translate-posframe--cache (make-hash-table :test 'equal))
        (google-translate-posframe-cache-ttl 3600))
    (google-translate-posframe--cache-put "en" "ru" "hello" "привет")
    (should (string-equal
             "привет"
             (google-translate-posframe--cache-get "en" "ru" "hello")))))

(ert-deftest test-google-translate-posframe--cache-miss ()
  "Test that cache returns nil for missing entries."
  (let ((google-translate-posframe--cache (make-hash-table :test 'equal)))
    (should (null (google-translate-posframe--cache-get "en" "ru" "missing")))))

(ert-deftest test-google-translate-posframe--cache-expired ()
  "Test that expired cache entries are evicted and return nil."
  (let ((google-translate-posframe--cache (make-hash-table :test 'equal))
        (google-translate-posframe-cache-ttl 0))
    (google-translate-posframe--cache-put "en" "ru" "hello" "привет")
    ;; TTL is 0, so the entry is immediately expired
    (sleep-for 0.01)
    (should (null (google-translate-posframe--cache-get "en" "ru" "hello")))
    ;; Expired entry should be removed from the hash table
    (should (= 0 (hash-table-count google-translate-posframe--cache)))))

(ert-deftest test-google-translate-posframe--cache-different-langs ()
  "Test that cache differentiates by language pair."
  (let ((google-translate-posframe--cache (make-hash-table :test 'equal))
        (google-translate-posframe-cache-ttl 3600))
    (google-translate-posframe--cache-put "en" "ru" "hello" "привет")
    (google-translate-posframe--cache-put "en" "de" "hello" "hallo")
    (should (string-equal
             "привет"
             (google-translate-posframe--cache-get "en" "ru" "hello")))
    (should (string-equal
             "hallo"
             (google-translate-posframe--cache-get "en" "de" "hello")))))

(ert-deftest test-google-translate-posframe-clear-cache ()
  "Test that clearing the cache removes all entries."
  (let ((google-translate-posframe--cache (make-hash-table :test 'equal))
        (google-translate-posframe-cache-ttl 3600))
    (google-translate-posframe--cache-put "en" "ru" "hello" "привет")
    (google-translate-posframe--cache-put "en" "de" "hello" "hallo")
    (google-translate-posframe-clear-cache)
    (should (= 0 (hash-table-count google-translate-posframe--cache)))))

;;; Text truncation tests

(ert-deftest test-google-translate-posframe--truncate-short-text ()
  "Short text should be returned unchanged."
  (let ((google-translate-posframe-max-paragraph-length 1000))
    (should (string-equal
             "Short text."
             (google-translate-posframe--truncate-to-max-length "Short text.")))))

(ert-deftest test-google-translate-posframe--truncate-long-text ()
  "Long text should be truncated by removing trailing sentences."
  (let* ((google-translate-posframe-max-paragraph-length 50)
         (original "Short. Also short. Another one. And the final sentence that makes it long.")
         (result (google-translate-posframe--truncate-to-max-length original)))
    (should (< (length result) (length original)))
    (should (string-prefix-p "Short" result))))

;;; Text detection tests

(ert-deftest test-google-translate-posframe--get-text-word-at-point ()
  "Should return word at point when nothing else matches."
  (with-temp-buffer
    ;; Use multiple paragraphs so the cursor is mid-paragraph, not at boundary.
    (insert "First paragraph with enough text to avoid boundary.\n\n")
    (insert "Some random words here in a second paragraph.\n\n")
    (insert "Third paragraph for padding.\n")
    ;; Position on "random" in the second paragraph
    (goto-char (+ (search-backward "random") 1))
    (should (string-equal
             "random"
             (google-translate-posframe--get-text-to-translate)))))

(ert-deftest test-google-translate-posframe--get-text-active-region ()
  "Should return selected region text."
  (with-temp-buffer
    (insert "select this text please")
    (goto-char 8)
    (set-mark 8)
    (goto-char 17)
    (activate-mark)
    (should (string-equal
             "this text"
             (google-translate-posframe--get-text-to-translate)))
    (deactivate-mark)))

(ert-deftest test-google-translate-posframe--get-text-empty-buffer ()
  "Should return nil or empty in an empty buffer."
  (with-temp-buffer
    (let ((result (google-translate-posframe--get-text-to-translate)))
      (should (or (null result)
                  (string-empty-p result))))))

(ert-deftest test-google-translate-posframe--get-paragraph-text-basic ()
  "Should return the current paragraph text."
  (with-temp-buffer
    (insert "First paragraph text here.\n\nSecond paragraph text here.\n")
    (goto-char 5) ; inside first paragraph
    (let ((text (google-translate-posframe--get-paragraph-text)))
      (should (string-match-p "First paragraph" text)))))

(ert-deftest test-google-translate-posframe--at-paragraph-boundary-p/beginning ()
  "Should detect paragraph beginning."
  (with-temp-buffer
    (insert "\n\nHello world.\n\n")
    (goto-char 3) ; at 'H'
    (should (google-translate-posframe--at-paragraph-boundary-p))))

;;; Formatting tests

(ert-deftest test-google-translate-posframe--format-translation ()
  "Should include translation and language direction footer."
  (let ((google-translate-posframe-max-width 80)
        (result (google-translate-posframe--format-translation
                 "hello" "привет" "en" "ru")))
    (should (string-match-p "привет" result))
    (should (string-match-p "en → ru" result))))

(ert-deftest test-google-translate-posframe--format-translation-wraps ()
  "Long translations should be wrapped at max-width."
  (let* ((google-translate-posframe-max-width 20)
         (long-translation (make-string 60 ?a))
         (result (google-translate-posframe--format-translation
                  "test" long-translation "en" "ru")))
    ;; Should contain newlines from wrapping
    (should (string-match-p "\n" result))))

;;; Fetch with cache tests

(ert-deftest test-google-translate-posframe--fetch-uses-cache ()
  "Fetch should use cached result without calling `google-translate-request'."
  (let ((google-translate-posframe--cache (make-hash-table :test 'equal))
        (google-translate-posframe-cache-ttl 3600)
        (callback-called nil)
        (callback-args nil))
    (google-translate-posframe--cache-put "en" "ru" "hello" "привет")
    (google-translate-posframe--fetch-translation
     "en" "ru" "hello"
     (lambda (text translation)
       (setq callback-called t)
       (setq callback-args (list text translation))))
    (should callback-called)
    (should (equal callback-args '("hello" "привет")))))

;;; Cache key tests

(ert-deftest test-google-translate-posframe--cache-key-structure ()
  "Cache keys should be distinct for different inputs."
  (let ((key1 (google-translate-posframe--cache-key "en" "ru" "hello"))
        (key2 (google-translate-posframe--cache-key "en" "de" "hello"))
        (key3 (google-translate-posframe--cache-key "en" "ru" "world")))
    (should-not (equal key1 key2))
    (should-not (equal key1 key3))
    ;; Same inputs should produce equal keys
    (should (equal key1 (google-translate-posframe--cache-key "en" "ru" "hello")))))

(provide 'google-translate-posframe-test)
;;; google-translate-posframe-test.el ends here
