;; (define-step "I translate (via default UI) `text' from `source-language' to `target-language'"
;;   (And "I start an action chain")
;;   (And "I press \"M-x\"")
;;   (And "I type \"google-translate-query-translate\"")
;;   ...)

(When "I translate (via default UI) :arg-1 from :arg-2 to :arg-3"
      (lambda (text source-language target-language)
        (And "I start an action chain")
        (And "I press \"M-x\"")
        (And "I type \"google-translate-query-translate\"")
        (And "I press \"RET\"")
        (when (and (stringp source-language)
                   (> (length source-language) 0))
          (when (not (equal source-language "auto"))
            (And (format "I type \"%s\""
                         (google-translate-language-display-name source-language))))
          (And "I press \"RET\""))
        (when (and (stringp target-language)
                   (> (length target-language) 0))
          (And (format "I type \"%s\""
                       (google-translate-language-display-name target-language)))
          (And "I press \"RET\""))
        (And (format "I type \"%s\"" text))
        (And "I press \"RET\"")
        (And "I execute the action chain")))

(When "I translate :arg-1"
  (lambda (text)
    (Given (format "I translate \"%s\" from \"\" to \"\"" text))))

(When "I translate :arg-1 to :arg-2"
  (lambda (text target-language)
    (Given (format "I translate \"%s\" from \"\" to \"%s\"" text target-language))))

(When "I translate :arg-1 from :arg-2"
  (lambda (text source-language)
    (Given (format "I translate \"%s\" from \"%s\" to \"\"" text source-language))))

(When "I translate (via default UI) thing at point from :arg-1 to :arg-2"
      (lambda (source-language target-language)
        (And "I start an action chain")
        (And "I press \"M-x\"")
        (And "I type \"google-translate-at-point\"")
        (And "I press \"RET\"")
        (when (and (stringp source-language)
                   (> (length source-language) 0))
          (when (not (equal source-language "auto"))
            (And (format "I type \"%s\""
                         (google-translate-language-display-name
                          source-language))))
          (And "I press \"RET\""))
        (when (and (stringp target-language)
                   (> (length target-language) 0))
          (And (format "I type \"%s\""
                       (google-translate-language-display-name
                        target-language)))
          (And "I press \"RET\""))
        (And "I execute the action chain")))

(When "I reverse translate :arg-1"
      (lambda (text)
        (And "I start an action chain")
        (And "I press \"M-x\"")
        (And "I type \"google-translate-query-translate-reverse\"")
        (And "I press \"RET\"")
        (And (format "I type \"%s\"" text))
        (And "I press \"RET\"")
        (And "I execute the action chain")))

(When "I reverse translate word at point"
      (lambda ()
        (And "I start an action chain")
        (And "I press \"M-x\"")
        (And "I type \"google-translate-at-point-reverse\"")
        (And "I press \"RET\"")
        (And "I execute the action chain")))

(When "I translate word at point from :arg-1"
  (lambda (source-language)
    (When (format "I translate thing at point from \"%s\" to \"\"" 
                  source-language))))

(When "I translate word at point to :arg-1"
  (lambda (target-language)
    (When (format "I translate thing at point from \"\" to \"%s\"" 
                  target-language))))

(When "I translate word at point"
  (lambda ()
    (When (format "I translate thing at point from \"\" to \"\""))))

(When "I translate selected region from :arg-1 to :arg-2"
  (lambda (source-language target-language)
    (When (format "I translate thing at point from \"%s\" to \"%s\"" 
                  source-language
                  target-language))))

(Then "I should see translation :arg-1"
  (lambda (translation)
    (Given "I am in buffer \"*Google Translate*\"")
    (Then (format "I should see \"%s\"" translation))))

