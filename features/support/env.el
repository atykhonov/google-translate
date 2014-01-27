(require 'f)

(defvar google-translate-support-path
  (f-dirname load-file-name))

(defvar google-translate-features-path
  (f-parent google-translate-support-path))

(defvar google-translate-root-path
  (f-parent google-translate-features-path))

(add-to-list 'load-path google-translate-root-path)

(require 'google-translate)
(require 'google-translate-default-ui)
(require 'google-translate-smooth-ui)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
