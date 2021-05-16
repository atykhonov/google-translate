(require 'dired)

(defmacro with-cache-context (&rest body)
  "Set up some variable, run BODY and clean up."
  `(let* ((google-translate-use-cache t)
          (source "tmp-test-source")
          (target "tmp-test-target")
          (prefix (google-translate--cache-expand-rpath (concat source "/" target "/"))))
     ;; When a directory doesn't already exist, emacs asks user for permission to create it.
     ;; That won't work as part of a test.
     ;; So have to ensure the existence manually here.
     (unwind-protect
         (progn (unless (f-exists-p prefix)
                  (dired-create-directory prefix))
                ,@body)
       (progn
         (delete-directory (google-translate--cache-expand-rpath source) t)
         (setf google-translate-cache-files nil)))))

(ert-deftest test-google-translate-cache-save-and-load ()
  "Test cache addition/retrieval + file saving/loading."
  (with-cache-context
   (let* ((key "test-word")
          (translation "test-translation")
          (cache-file-path (concat prefix "cache-" (int-to-string (google-translate--cache-get-hash key)))))
     ;; add
     (google-translate-cache-add source target key translation)
     ;; save to file
     (google-translate-cache-save)
     (should (file-exists-p cache-file-path))
     (should (file-exists-p (concat prefix "max-files")))
     (setf google-translate-cache-files nil)
     ;; load and retrieve
     (should (equal (google-translate-cache-get source target key)
                    translation)))))

(ert-deftest test-google-translate-cache-rebuild ()
  "Test cache rebuilding (when `google-translate-cache-files-per-language' changes)."
  (with-cache-context
    (let ((google-translate-cache-files-per-language 100)
          (pairs '(("a" "b") ("c" "d") ("e" "f") ("rnsotiearst" "rsteer"))))
      (dolist (x pairs)
        (google-translate-cache-add source target (car x) (cadr x)))
      (google-translate-cache-save)
      (setf google-translate-cache-files-per-language 5)
      (dolist (x pairs)
        (should (equal (google-translate-cache-get source target (car x))
                       (cadr x)))))))
