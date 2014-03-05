(require 'f)
(require 's)
(require 'dash)
(require 'el-mock)

(defvar google-translate-test/test-path
  (if (null load-file-name)
      (f-dirname (buffer-file-name))
    (f-dirname load-file-name)))

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

(require 'google-translate-default-ui
         (f-expand "google-translate-default-ui"
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

(defun th-google-translate-fixture-suggestion (fixture)
  (with-current-buffer (th-google-translate-temp-buffer fixture)
    (goto-char (point-min))
    (search-forward "suggestion:")
    (forward-line 1)
    (let ((result (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
      (if (equal result "")
          nil
        result))))

(defun th-google-translate-fixture-detailed-translation (fixture)
  (with-current-buffer (th-google-translate-temp-buffer fixture)
    (goto-char (point-min))
    (search-forward "detailed-translation:")
    (let ((line " ") (result '()))
      (while (not (equal line ""))
        (forward-line 1)
        (setq line (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position)))
        (setq result (append result `(,line))))
      result)))

(defun th-google-translate-detailed-translation-to-string (detailed-translation)
  (unless (null detailed-translation)
    (loop for item across detailed-translation do
          (unless (string-equal (aref item 0) "")
            (insert (format "%s\n" (aref item 0)))
            (loop for translation across (aref item 1) do
                  (insert (format "%s\n" translation)))))))



(defun th-google-translate-generate-fixtures ()
  (interactive)
  (th-google-translate-generate-word-fixtures)
  (th-google-translate-generate-sentence-fixtures))

(defun th-google-translate-generate-word-fixtures ()
  (interactive)
  (th-google-translate-generate-word-fixture "return" "en" "ru" 1)
  (th-google-translate-generate-word-fixture "belongs" "en" "ru" 2)
  (th-google-translate-generate-word-fixture "first" "en" "ru" 3)
  (th-google-translate-generate-word-fixture "колобок" "ru" "uk" 4)
  (th-google-translate-generate-word-fixture "sucesful" "en" "ru" 5)
  (th-google-translate-generate-word-fixture "развести" "ru" "en" 6)
  (th-google-translate-generate-word-fixture "разочарование" "ru" "uk" 7))

(defun th-google-translate-generate-sentence-fixtures ()
  (interactive)
  (th-google-translate-generate-sentence-fixture "спочатку було слово" "uk" "en" 1)
  (th-google-translate-generate-sentence-fixture "век живи, век учись" "ru" "uk" 2))

(defun th-google-translate-generate-word-fixture (word
                                                  source-language
                                                  target-language
                                                  fixture-num)
  (interactive "sWord: \nsFrom: \nsTo: \nsFixture-num: ")
  (th-google-translate-generate-fixture
   word
   source-language
   target-language
   fixture-num
   google-translate-test/word-fixture-path))

(defun th-google-translate-generate-sentence-fixture (word
                                                  source-language
                                                  target-language
                                                  fixture-num)
  (interactive "sSentence: \nsFrom: \nsTo: \nsFixture-num: ")
  (th-google-translate-generate-fixture
   word
   source-language
   target-language
   fixture-num
   google-translate-test/sentence-fixture-path))

(defun th-google-translate-generate-fixture (word 
                                             source-language
                                             target-language
                                             fixture-num
                                             fixture-dir-path)
  (interactive "sWord: \nsFrom: \nsTo: \nsFixture-num: ")
  (let* ((response (with-current-buffer 
                       (google-translate--request
                        source-language
                        target-language
                        word
                        t)
                     (buffer-string)))
         (json (json-read-from-string
                (google-translate--insert-nulls 
                 (with-temp-buffer
                   (insert response)
                   (goto-char (point-min))
                   (re-search-forward (format "\n\n"))
                   (delete-region (point-min) (point))
                   (buffer-string))))))
    (with-temp-buffer
      (insert (format "%s\n\n\n" word))
      (insert response)
      (insert (format "\n\n%s\n" "text-phonetic:"))
      (let ((text-phonetic (google-translate-json-text-phonetic json)))
        (when (null text-phonetic)
          (setq text-phonetic ""))
        (insert (format "%s\n" text-phonetic)))
      (insert (format "\n%s\n" "translation-phonetic:"))
      (let ((translation-phonetic (google-translate-json-translation-phonetic json)))
        (when (null translation-phonetic)
          (setq translation-phonetic ""))
        (insert (format "%s\n" translation-phonetic)))
      (insert (format "\n%s\n" "translation:"))
      (let ((translation (google-translate-json-translation json)))
        (when (null translation)
          (setq translation ""))
        (insert (format "%s\n" translation)))
      (insert (format "\n%s\n" "suggestion:"))
      (let ((suggestion (google-translate-json-suggestion json)))
        (when (null suggestion)
          (setq suggestion ""))
        (insert (format "%s\n" suggestion)))
      (insert (format "\n%s\n" "detailed-translation:"))
      (let ((detailed-translation (google-translate-json-detailed-translation json)))
        (unless (null detailed-translation)
          (loop for item across detailed-translation do
                (unless (string-equal (aref item 0) "")
                  (insert (format "%s\n" (aref item 0)))
                  (loop for translation across (aref item 1) do
                        (insert (format "%s\n" translation)))))))
      (write-file
       (concat 
        fixture-dir-path
        "/"
        (format "%s.fixture" fixture-num))))))
