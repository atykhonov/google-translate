(ert-deftest test-google-translate--insert-nulls ()
  (should (string-equal
           (google-translate--insert-nulls "[,[,[,,],,],,]")
           "[null,[null,[null,null,null],null,null],null,null]")))

(ert-deftest test-google-translate-request-words-fixtures ()
  (dolist (file (f-files google-translate-test/word-fixture-path))
    (let ((fixture (th-google-translate-load-fixture file)))
      (th-google-translate-request-fixture fixture)
      ;; assertions are skipped. In case of no errors assume that test pass.
      )))

(ert-deftest test-google-translate-request-sentences-fixtures ()
  (dolist (file (f-files google-translate-test/sentence-fixture-path))
    (let ((fixture (th-google-translate-load-fixture file)))
      (th-google-translate-request-fixture fixture)
      ;; assertions are skipped. In case of no errors assume that test pass.
      )))

(ert-deftest test-google-translate--strip-string-with-spaces ()
  (should (string-equal
           (google-translate--strip-string "    spaces     spaces ")
           " spaces spaces ")))

(ert-deftest test-google-translate--strip-string-with-carriage-return-and-line-feeds ()
  (should (string-equal
           (google-translate--strip-string "\n\n\r\nspaces\r\n\n\r\r\n\nspaces\r\n\r\r\r\n")
           " spaces spaces ")))

(ert-deftest test-google-translate--trim-string-with-spaces ()
  (should (string-equal
           (google-translate--trim-string "    spaces   spaces     ")
           "spaces   spaces")))

(ert-deftest test-google-translate-prepare-text-for-request ()
  (should (string-equal
           (google-translate-prepare-text-for-request "\n\r\nspaces\r   \n\n\rspaces\n\n\r")
           "spaces spaces")))

(ert-deftest test-google-translate-json-text-phonetic ()
  (dolist (file (f-files google-translate-test/word-fixture-path))
    (let ((fixture (th-google-translate-load-fixture file)))
      (should (string-equal
               (th-google-translate-fixture-text-phonetic fixture)
               (google-translate-json-text-phonetic
                (th-google-translate-request-fixture fixture)))))))

(ert-deftest test-google-translate-json-translation-phonetic ()
  (dolist (file (f-files google-translate-test/word-fixture-path))
    (let ((fixture (th-google-translate-load-fixture file)))
      (should (string-equal
               (th-google-translate-fixture-translation-phonetic fixture)
               (google-translate-json-translation-phonetic
                (th-google-translate-request-fixture fixture)))))))

(ert-deftest test-google-translate-json-translation ()
  (dolist (file (f-files google-translate-test/word-fixture-path))
    (let ((fixture (th-google-translate-load-fixture file)))
      (should (string-equal
               (th-google-translate-fixture-translation fixture)
               (google-translate-json-translation
                (th-google-translate-request-fixture fixture)))))))

(ert-deftest test-google-translate-json-detailed-translation ()
  (dolist (file (f-files google-translate-test/word-fixture-path))
    (let* ((fixture (th-google-translate-load-fixture file))
           (index 0)
           (detailed-translation (google-translate-json-detailed-translation
                                  (th-google-translate-request-fixture fixture)))
           (fixture-dt (th-google-translate-fixture-detailed-translation fixture))
           (detailed-translation-str "")
           (fixture-dt-str ""))
      (when fixture-dt
        (with-temp-buffer
          (th-google-translate-detailed-translation-to-string detailed-translation)
          (setq detailed-translation-str (buffer-substring-no-properties (point-min) (point-max))))
        ;; (with-temp-buffer
        ;;   (th-google-translate-detailed-translation-to-string fixture-dt)
        ;;   (setq fixture-dt-str (buffer-substring-no-properties (point-min) (point-max))))
        (setq fixture-dt-str (mapconcat (lambda (w) w) fixture-dt "\n"))
        (should (string-equal
                 detailed-translation-str
                 fixture-dt-str))))))
        ;; (loop for item across detailed-translation do
        ;;       (let ((index 0))
        ;;         (unless (string-equal (aref item 0) "")
        ;;           (should (string-equal (aref fixture-dt index)
        ;;                                 (aref item 0)))
        ;;           (loop for translation across (aref item 1) do
        ;;                 (incf index)
        ;;                 (should (string-equal (aref fixture-dt index)
        ;;                                       translation))))))))))

(ert-deftest test-google-translate-request-empty-text ()
  (should (null
           (google-translate-request "en" "ru" ""))))

(defvar test-example-query "client=t&ie=UTF-8&oe=UTF-8&sl=en&tl=ru&text=first")

(defvar test-example-query-params '(("client" . "t")
                                    ("ie"     . "UTF-8")
                                    ("oe"     . "UTF-8")
                                    ("sl"     . "en")
                                    ("tl"     . "ru")
                                    ("text"   . "first")))

(ert-deftest test-google-translate--format-query-string ()
  (should (string-equal
           test-example-query
           (google-translate--format-query-string
            test-example-query-params))))

(ert-deftest test-google-translate--format-request-url ()
  (should (string-equal
           (concat google-translate-base-url "?" test-example-query)
           (google-translate--format-request-url test-example-query-params))))

;; (ert-deftest test-google-translate--http-response-body ()
;;   (with-mock
;;    (let* ((fixture-response (th-google-translate-fixture-response
;;                              (th-google-translate-load-fixture 
;;                               (f-expand 
;;                                "1.fixture" ;; any fixture
;;                                google-translate-test/word-fixture-path))))
;;           (json-response (with-temp-buffer
;;                            (insert fixture-response)
;;                            (let ((beg (goto-char (point-min))))
;;                              (re-search-forward (format "\n\n"))
;;                              (buffer-substring-no-properties beg (point))))))

;;      (stub url-retrieve-synchronously =>
;;            (th-google-translate-temp-buffer fixture-response))
;;      (should (string-equal
;;               json-response
;;               (google-translate-request "en" "ru" "return"))))))
