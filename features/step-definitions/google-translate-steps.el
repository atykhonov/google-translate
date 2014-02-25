(defvar it-google-translate-current-ui "")

(define-step "`ui' UI"
  (setq it-google-translate-current-ui ui))

(define-step "I translate `text' from `source-language' to `target-language'"
  (When
   (format
    "I translate (via %s UI) `text' from `source-language' to `target-language'"
    it-google-translate-current-ui)
   text
   source-language
   target-language))

(define-step "I translate thing at point from `source-language' to `target-language'"
  (When
   (format
    "I translate (via %s UI) thing at point from `source-language' to `target-language'"
    it-google-translate-current-ui)
   source-language
   target-language))

(define-step "I translate word at point from `source-language' to `target-language'"
  (When
   (format
    "I translate (via %s UI) thing at point from `source-language' to `target-language'"
    it-google-translate-current-ui)
   source-language
   target-language))

(define-step "I should see translation `translation'"
  (Given "I am in buffer `buffer'" "*Google Translate*")
  (Then "I should see `translation'" translation))

(define-step "I translate `text'"
  (Then (format "I translate (via %s UI) `text'"
                it-google-translate-current-ui)
        text))
