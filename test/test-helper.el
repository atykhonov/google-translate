(require 'f)
(require 's)
(require 'dash)
(require 'el-mock)

(defvar google-translate-test/test-path
  (f-dirname load-file-name))

(defvar google-translate-test/root-path
  (f-parent google-translate-test/test-path))

(setq debug-on-entry t)
(setq debug-on-error t)

(add-to-list 'load-path google-translate-test/root-path)

(require 'google-translate
         (f-expand "google-translate"
                   google-translate-test/root-path))
