(defvar it-google-translate-current-ui "")

(Given "^\\(.+\\) UI$"
       (lambda (ui)
         (setq it-google-translate-current-ui ui)))

(When "^I translate \"\\([^\"]*\\)\" from \"\\([^\"]*\\)\" to \"\\([^\"]*\\)\"$"
      (lambda (text source-language target-language)
        (When 
         (format 
          "I translate (via %s UI) \"%s\" from \"%s\" to \"%s\""
          it-google-translate-current-ui
          text
          source-language
          target-language))))

(When "^I translate thing at point from \"\\([^\"]*\\)\" to \"\\([^\"]*\\)\"$"
      (lambda (source-language target-language)
        (When 
         (format
          "I translate (via %s UI) thing at point from \"%s\" to \"%s\""
          it-google-translate-current-ui
          source-language
          target-language))))

(When "^I translate word at point from \"\\([^\"]*\\)\" to \"\\([^\"]*\\)\"$"
      (lambda (source-language target-language)
        (When (format "I translate (via %s UI) thing at point from \"%s\" to \"%s\"" 
                      it-google-translate-current-ui
                      source-language
                      target-language))))

(Then "^I should see translation \"\\(.+\\)\"$"
      (lambda (translation)
        (Given "I am in buffer \"*Google Translate*\"")
        (Then (format "I should see \"%s\"" translation))))

(Then "^I should see suggestion \"\\(.+\\)\"$"
      (lambda (translation)
        (Given "I am in buffer \"*Google Translate*\"")
        (Then (format "I should see \"%s\"" translation))))
