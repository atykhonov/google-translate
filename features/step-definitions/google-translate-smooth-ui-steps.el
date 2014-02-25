(defun translate-text-smooth-ui (text &optional source-language target-language)
  (And "I start an action chain")
  (And "I press 'M-x")
  (And "I type 'google-translate-smooth-translate")
  (And "I press 'RET")
  (when (and (stringp text)
             (> (length text) 0))
    (delete-minibuffer-contents)
    (And "I type `text'" text))
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
  (And "I execute the action chain"))

(define-step "I translate (via smooth UI) `text' from `source-language' to `target-language'"
  (translate-text-smooth-ui text source-language target-language))

(define-step "I translate (via smooth UI) `text'"
  (translate-text-smooth-ui text))

(define-step "I translate (via smooth UI) thing at point from `source-language' to `target-language'"
  (When "I translate `text' from `source-language' to `target-language'"
        "" source-language target-language))

(define-step "I translate `text' using `nth-direction' direction"
  (And "I start an action chain")
  (And "I press 'M-x")
  (And "I type 'google-translate-smooth-translate")
  (And "I press 'RET")
  (when (and (stringp text)
             (> (length text) 0))
    (delete-minibuffer-contents)
    (And "I type `text'" text))
  (let ((index 0)
        (nth-direction (string-to-int nth-direction)))
    (if (> nth-direction 0)
        (while (< index nth-direction)
          (And "I press 'C-n")
          (incf index))
      (while (> index nth-direction)
        (And "I press 'C-p")
        (decf index))))
  (And "I press 'RET")
  (And "I execute the action chain"))
