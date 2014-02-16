(defvar it-google-translate-current-ui "")

(Given ":arg-1 UI"
       (lambda (ui)
         (setq it-google-translate-current-ui ui)))

(When "I translate :arg-1 from :arg-2 to :arg-3"
      (lambda (text source-language target-language)
        (When (format "I translate (via %s UI) \"%s\" from \"%s\" to \"%s\""
                      it-google-translate-current-ui
                      text
                      source-language
                      target-language))))

(When "I translate thing at point from :arg-1 to :arg-2"
      (lambda (source-language target-language)
        (When 
         (format
          "I translate (via %s UI) thing at point from \"%s\" to \"%s\""
          it-google-translate-current-ui
          source-language
          target-language))))

(When "I translate word at point from :arg-1 to :arg-2"
      (lambda (source-language target-language)
        (When (format "I translate (via %s UI) thing at point from \"%s\" to \"%s\"" 
                      it-google-translate-current-ui
                      source-language
                      target-language))))

(Then "I should see translation :arg-1"
      (lambda (translation)
        (Given "I am in buffer \"*Google Translate*\"")
        (Then (format "I should see \"%s\"" translation))))
