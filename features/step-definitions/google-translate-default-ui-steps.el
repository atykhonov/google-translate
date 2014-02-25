(defun translate-text-default-ui (text &optional source-language target-language)
  (And "I start an action chain")
  (And "I press 'M-x")
  (And "I type 'google-translate-query-translate")
  (And "I press 'RET")
  (when (and (stringp source-language)
             (> (length source-language) 0))
    (when (not (equal source-language "auto"))
      (And "I type `text'"
           (google-translate-language-display-name source-language)))
    (And "I press 'RET"))
  (when (and (stringp target-language)
             (> (length target-language) 0))
    (And "I type `text'"
         (google-translate-language-display-name target-language))
    (And "I press 'RET"))
  (And "I type `text'" text)
  (And "I press 'RET")
  (And "I execute the action chain"))

(define-step "I translate (via default UI) `text' from `source-language' to `target-language'"
  (translate-text-default-ui text source-language target-language))

(define-step "I translate (via default UI) `text'"
  (translate-text-default-ui text))

(define-step "I translate `text' to `target-language'"
  (translate-text-default-ui text "" target-language))

(define-step "I translate `text' from `source-language'"
  (translate-text-default-ui text source-language ""))

(defun translate-thing-at-point-default-ui (&optional source-language target-language)
  (And "I start an action chain")
  (And "I press 'M-x")
  (And "I type 'google-translate-at-point")
  (And "I press 'RET")
  (when (and (stringp source-language)
             (> (length source-language) 0))
    (when (not (equal source-language "auto"))
      (And "I type `text'"
           (google-translate-language-display-name
            source-language)))
    (And "I press 'RET"))
  (when (and (stringp target-language)
             (> (length target-language) 0))
    (And "I type `text'"
         (google-translate-language-display-name
          target-language))
    (And "I press 'RET"))
  (And "I execute the action chain"))

(define-step "I translate (via default UI) thing at point from `source-language' to `target-language'"
  (translate-thing-at-point-default-ui source-language target-language))

(define-step "I translate word at point from `source-language'"
  (translate-thing-at-point-default-ui source-language))

(define-step "I translate word at point to `target-language'"
  (translate-thing-at-point-default-ui "" target-language))

(define-step "I translate word at point"
  (translate-thing-at-point-default-ui))

(define-step "I translate selected region from `source-language' to `target-language'"
  (translate-thing-at-point-default-ui source-language target-language))

(define-step "I reverse translate word at point"
  (And "I start an action chain")
  (And "I press 'M-x")
  (And "I type 'google-translate-at-point-reverse")
  (And "I press 'RET")
  (And "I execute the action chain"))

(define-step "I reverse translate `text'"
  (And "I start an action chain")
  (And "I press 'M-x")
  (And "I type 'google-translate-query-translate-reverse")
  (And "I press 'RET")
  (And "I type `text'" text)
  (And "I press 'RET")
  (And "I execute the action chain"))
