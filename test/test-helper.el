(require 'f)
(require 's)
(require 'dash)
(require 'el-mock)

(defvar google-translate-test/test-path
  (f-dirname load-file-name))

(defvar google-translate-test/fixture-path
  (f-expand "fixtures" google-translate-test/test-path))

(defvar google-translate-test/word-fixture-path
  (f-expand "word" google-translate-test/fixture-path))

(defvar google-translate-test/phrase-fixture-path
  (f-expand "phrase" google-translate-test/fixture-path))

(defvar google-translate-test/sentence-fixture-path
  (f-expand "sentence" google-translate-test/fixture-path))

(defvar google-translate-test/root-path
  (f-parent google-translate-test/test-path))

(setq debug-on-entry t)
(setq debug-on-error t)

(add-to-list 'load-path google-translate-test/root-path)

(require 'google-translate
         (f-expand "google-translate"
                   google-translate-test/root-path))

(defun th-google-translate-load-fixture (file)
  (with-temp-buffer
    (insert-file-contents file)
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (re-search-forward (format "\n\n\n"))
    (delete-region (point-min) (point))
    (buffer-string)))

(defun th-google-translate-fixture-response (fixture)
  (with-temp-buffer
    (insert fixture)
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (let ((stpoint (point)))
      (re-search-forward (format "\n\n"))
      (re-search-forward (format "\n\n"))
      (buffer-substring-no-properties stpoint (point)))))

(defun th-google-translate-temp-buffer (contents)
  (let ((buffer (get-buffer-create "*Google Translate Temp Buffer*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert contents)
      buffer)))

(defun th-google-translate-request-fixture (fixture)
  (with-mock
   (stub url-retrieve-synchronously =>
         (th-google-translate-temp-buffer
          (th-google-translate-fixture-response fixture)))
   (google-translate-request "en" "ru" "return")))

(defun th-google-translate-fixture-text-phonetic (fixture)
  (with-current-buffer (th-google-translate-temp-buffer fixture)
    (goto-char (point-min))
    (search-forward "text-phonetic:")
    (forward-line 1)
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

(defun th-google-translate-fixture-translation-phonetic (fixture)
  (with-current-buffer (th-google-translate-temp-buffer fixture)
    (goto-char (point-min))
    (search-forward "translation-phonetic:")
    (forward-line 1)
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

(defun th-google-translate-fixture-translation (fixture)
  (with-current-buffer (th-google-translate-temp-buffer fixture)
    (goto-char (point-min))
    (search-forward "translation:")
    (forward-line 1)
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

(defun th-google-translate-fixture-detailed-translation (fixture)
  (with-current-buffer (th-google-translate-temp-buffer fixture)
    (goto-char (point-min))
    (search-forward "detailed-translation:")
    (let ((line " ") (result '()))
      (while (not (equal line ""))
        (forward-line 1)
        (setq line (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position)))
        (setq result (append result '(,line)))))))
