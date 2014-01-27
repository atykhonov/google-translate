(When "^I translate (via smooth UI) \"\\([^\"]*\\)\" from \"\\([^\"]*\\)\" to \"\\([^\"]*\\)\"$"
      (lambda (text source-language target-language)
        (And "I start an action chain")
        (And "I press \"M-x\"")
        (And "I type \"google-translate-smooth-translate\"")
        (And "I press \"RET\"")
        (when (and (stringp text)
                   (> (length text) 0))
          (delete-minibuffer-contents)
          (And "I type \"%s\"" text))
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
        (And "I execute the action chain")))

(When "^I translate (via smooth UI) thing at point from \"\\([^\"]*\\)\" to \"\\([^\"]*\\)\"$"
      (lambda (source-language target-language)
        (When (format "I translate \"\" from \"%s\" to \"%s\"" 
                      source-language
                      target-language))))

(When "^I translate \"\\([^\"]*\\)\" using \\(.+\\) direction$"
      (lambda (text nth-direction)
        (And "I start an action chain")
        (And "I press \"M-x\"")
        (And "I type \"google-translate-smooth-translate\"")
        (And "I press \"RET\"")
        (when (and (stringp text)
                   (> (length text) 0))
          (delete-minibuffer-contents)
          (And "I type \"%s\"" text))
        (let ((index 0)
              (nth-direction (string-to-int nth-direction)))
          (if (> nth-direction 0)
              (while (< index nth-direction)
                (And "I press \"C-n\"")
                (incf index))
            (while (> index nth-direction)
              (And "I press \"C-p\"")
              (decf index))))
        (And "I press \"RET\"")
        (And "I execute the action chain")))

;; (When "^I translate \"\\([^\"]*\\)\" from \"\\([^\"]*\\)\" to \"\\([^\"]*\\)\"$"
;;       (lambda (text source-language target-language)
;;         (And "I start an action chain")
;;         (And "I press \"M-x\"")
;;         (And "I type \"google-translate-query-translate\"")
;;         (And "I press \"RET\"")
;;         (when (and (stringp source-language)
;;                    (> (length source-language) 0))
;;           (And (format "I type \"%s\""
;;                        (google-translate-language-display-name source-language)))
;;           (And "I press \"RET\""))
;;         (when (and (stringp target-language)
;;                    (> (length target-language) 0))
;;           (And (format "I type \"%s\""
;;                        (google-translate-language-display-name target-language)))
;;           (And "I press \"RET\""))
;;         (And (format "I type \"%s\"" text))
;;         (And "I press \"RET\"")
;;         (And "I execute the action chain")))

;; (When "^I translate \"\\([^\"]*\\)\"$"
;;   (lambda (text)
;;     (Given (format "I translate \"%s\" from \"\" to \"\"" text))))

;; (When "^I translate \"\\([^\"]*\\)\" to \"\\([^\"]*\\)\"$"
;;   (lambda (text target-language)
;;     (Given (format "I translate \"%s\" from \"\" to \"%s\"" text target-language))))

;; (When "^I translate \"\\([^\"]*\\)\" from \"\\([^\"]*\\)\"$"
;;   (lambda (text source-language)
;;     (Given (format "I translate \"%s\" from \"%s\" to \"\"" text source-language))))

;; (When "^I translate thing at point from \"\\([^\"]*\\)\" to \"\\([^\"]*\\)\"$"
;;       (lambda (source-language target-language)
;;         (And "I start an action chain")
;;         (And "I press \"M-x\"")
;;         (And "I type \"google-translate-at-point\"")
;;         (And "I press \"RET\"")
;;         (when (and (stringp source-language)
;;                    (> (length source-language) 0))
;;           (And (format "I type \"%s\""
;;                        (google-translate-language-display-name source-language)))
;;           (And "I press \"RET\""))
;;         (when (and (stringp target-language)
;;                    (> (length target-language) 0))
;;           (And (format "I type \"%s\""
;;                        (google-translate-language-display-name target-language)))
;;           (And "I press \"RET\""))
;;         (And "I execute the action chain")))

;; (When "^I translate word at point from \"\\([^\"]*\\)\" to \"\\([^\"]*\\)\"$"
;;   (lambda (source-language target-language)
;;     (When (format "I translate thing at point from \"%s\" to \"%s\"" 
;;                   source-language
;;                   target-language))))

;; (When "^I translate word at point from \"\\([^\"]*\\)\"$"
;;   (lambda (source-language)
;;     (When (format "I translate thing at point from \"%s\" to \"\"" 
;;                   source-language))))

;; (When "^I translate word at point to \"\\([^\"]*\\)\"$"
;;   (lambda (target-language)
;;     (When (format "I translate thing at point from \"\" to \"%s\"" 
;;                   target-language))))

;; (When "^I translate word at point$"
;;   (lambda ()
;;     (When (format "I translate thing at point from \"\" to \"\""))))

;; (When "^I translate selected region from \"\\(.+\\)\" to \"\\(.+\\)\"$"
;;   (lambda (source-language target-language)
;;     (When (format "I translate thing at point from \"%s\" to \"%s\"" 
;;                   source-language
;;                   target-language))))

;; (Then "^I should see translation \"\\(.+\\)\"$"
;;   (lambda (translation)
;;     (Given "I am in buffer \"*Google Translate*\"")
;;     (Then (format "I should see \"%s\"" translation))))
