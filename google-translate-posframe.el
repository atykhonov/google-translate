;;; google-translate-posframe.el --- Posframe popup UI for Google Translate -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ag Ibragimov

;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; URL: https://github.com/atykhonov/google-translate
;; Version: 0.12.0
;; Keywords: convenience text tools

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides a posframe-based popup UI for Google Translate.
;; Translations are shown in a child frame (posframe) near the point
;; or at the window corner, without switching buffers.
;;
;; There are two ways to use it:
;;
;; 1. Minor mode (`google-translate-posframe-mode') - automatically
;;    translates text at point based on context (region, paragraph,
;;    sentence, or word).  Translation updates as you move the cursor.
;;
;; 2. Explicit commands - translate on demand without enabling the
;;    minor mode.  The popup auto-dismisses on the next command.
;;
;;    - `google-translate-posframe-at-point'
;;    - `google-translate-posframe-region'
;;    - `google-translate-posframe-paragraph'
;;    - `google-translate-posframe-word'
;;    - `google-translate-posframe-dismiss'
;;
;; Requires `posframe' package and a graphical Emacs frame.
;;
;; Configuration example:
;;
;;   (require 'google-translate-posframe-ui)
;;   (setq google-translate-default-source-language "en")
;;   (setq google-translate-default-target-language "ru")
;;   (global-set-key (kbd "C-c t") #'google-translate-posframe-at-point)
;;

;;; Code:

(require 'google-translate-core)
(require 'google-translate-core-ui)
(require 'thingatpt)

(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-workable-p "posframe")
(declare-function posframe-poshandler-window-bottom-right-corner "posframe")

(declare-function evil-normal-state-p "evil-states")
(declare-function evil-visual-state-p "evil-states")

(declare-function org-at-heading-p "org")
(declare-function org-element-at-point "org-element")
(declare-function org-element-property "org-element")
(declare-function org-element-type "org-element")

(defvar google-translate-default-source-language)
(defvar google-translate-default-target-language)

;; Defined by `define-minor-mode' below, declared here to silence
;; the byte-compiler for references before the mode definition.
(defvar google-translate-posframe-mode)

;;; Customization

(defgroup google-translate-posframe nil
  "Posframe popup UI for Google Translate."
  :group 'google-translate-core-ui)

(defcustom google-translate-posframe-idle-delay 0.5
  "Seconds of idle time before fetching translation in minor mode.
Only applies when `google-translate-posframe-mode' is active."
  :type 'number
  :group 'google-translate-posframe)

(defcustom google-translate-posframe-max-paragraph-length 1000
  "Maximum character length of paragraph to translate automatically.
Paragraphs longer than this are truncated by removing trailing sentences."
  :type 'integer
  :group 'google-translate-posframe)

(defcustom google-translate-posframe-poshandler
  #'posframe-poshandler-window-bottom-right-corner
  "Poshandler function for positioning the translation popup.
Common options:
  `posframe-poshandler-window-bottom-right-corner' (default)
  `posframe-poshandler-point-bottom-left-corner' (below cursor)
  `posframe-poshandler-frame-center' (center of frame)"
  :type 'function
  :group 'google-translate-posframe)

(defcustom google-translate-posframe-max-width 80
  "Maximum width for the popup translation text."
  :type 'integer
  :group 'google-translate-posframe)

(defcustom google-translate-posframe-cache-ttl 3600
  "Time-to-live for cache entries in seconds.
Cached translations older than this are discarded."
  :type 'integer
  :group 'google-translate-posframe)

;;; Faces

(defface google-translate-posframe-face
  '((t :inherit default))
  "Face for the posframe translation popup text."
  :group 'google-translate-posframe)

(defface google-translate-posframe-border-face
  '((t :inherit font-lock-comment-face))
  "Face used for the popup border color."
  :group 'google-translate-posframe)

(defface google-translate-posframe-footer-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for the language direction footer in the popup."
  :group 'google-translate-posframe)

;;; Internal variables

(defvar google-translate-posframe--cache (make-hash-table :test 'equal)
  "Cache hash-table for translations.
Keys are (source-lang . (target-lang . text)),
values are (timestamp . translation).")

(defvar google-translate-posframe--buffer " *google-translate-posframe*"
  "Buffer name for the posframe translation popup.")

(defvar-local google-translate-posframe--pending-timer nil
  "Timer for pending translation fetch request.")

(defvar-local google-translate-posframe--last-text nil
  "Last text for which a popup was shown in this buffer.")

(defvar-local google-translate-posframe--oneshot-hooks-active nil
  "Whether one-shot auto-dismiss hooks are currently active.")

(defvar-local google-translate-posframe--source-buffer nil
  "Buffer that owns the currently visible posframe popup.")

(defvar google-translate-posframe--available-p nil
  "Non-nil when posframe is available and we are in a graphical frame.")

(setq google-translate-posframe--available-p
      (and (display-graphic-p)
           (require 'posframe nil t)))

;;; Cache functions

(defun google-translate-posframe--cache-key (source-lang target-lang text)
  "Create a cache key from SOURCE-LANG, TARGET-LANG, and TEXT."
  (cons source-lang (cons target-lang text)))

(defun google-translate-posframe--cache-get (source-lang target-lang text)
  "Look up cached translation for TEXT from SOURCE-LANG to TARGET-LANG.
Return the translation string if a valid entry exists, nil otherwise."
  (when-let* ((key (google-translate-posframe--cache-key source-lang target-lang text))
              (entry (gethash key google-translate-posframe--cache))
              (timestamp (car entry))
              (translation (cdr entry)))
    (if (< (- (float-time) timestamp) google-translate-posframe-cache-ttl)
        translation
      (remhash key google-translate-posframe--cache)
      nil)))

(defun google-translate-posframe--cache-put (source-lang target-lang text translation)
  "Store TRANSLATION for TEXT (SOURCE-LANG to TARGET-LANG) in cache."
  (let ((key (google-translate-posframe--cache-key source-lang target-lang text)))
    (puthash key (cons (float-time) translation) google-translate-posframe--cache)))

(defun google-translate-posframe-clear-cache ()
  "Clear the entire translation cache."
  (interactive)
  (clrhash google-translate-posframe--cache)
  (message "Google Translate posframe cache cleared."))

;;; Text detection

(defun google-translate-posframe--truncate-to-max-length (text)
  "Truncate TEXT to fit within `google-translate-posframe-max-paragraph-length'.
Removes complete sentences from the end until short enough.
Returns truncated text, or original if already within limits."
  (if (<= (length text) google-translate-posframe-max-paragraph-length)
      text
    (with-temp-buffer
      (insert text)
      (goto-char (point-max))
      (let ((sentence-end-double-space nil)
            (prev-point (point)))
        (while (and (> (buffer-size) google-translate-posframe-max-paragraph-length)
                    (> (point) (point-min)))
          (backward-sentence)
          (when (>= (point) prev-point)
            (goto-char (point-min)))
          (setq prev-point (point))
          (delete-region (point) (point-max))))
      (let ((result (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
        (if (string-empty-p result)
            (substring text 0 (min (length text) google-translate-posframe-max-paragraph-length))
          result)))))

(defun google-translate-posframe--heading-text ()
  "Return the clean heading title when point is on a heading line, else nil.
Handles Org headings and Markdown ATX/Setext headings, stripping the
markup markers and, for Org, TODO keywords, priorities and tags."
  (cond
   ((and (derived-mode-p 'org-mode)
         (fboundp 'org-at-heading-p)
         (org-at-heading-p))
    (let ((title (org-element-property :raw-value (org-element-at-point))))
      (and title (not (string-blank-p title)) (string-trim title))))
   ((derived-mode-p 'markdown-mode)
    (save-excursion
      (beginning-of-line)
      (cond
       ((looking-at "[ \t]*#+[ \t]+\\(.*?\\)[ \t]*#*[ \t]*$")
        (let ((title (match-string-no-properties 1)))
          (and (not (string-blank-p title)) (string-trim title))))
       ;; Setext heading: a non-blank title line underlined by = or -.
       ((and (not (looking-at "[ \t]*$"))
             (not (looking-at "[ \t]*[-=]+[ \t]*$"))
             (save-excursion (forward-line 1) (looking-at "[ \t]*[-=]+[ \t]*$")))
        (string-trim (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))))))

(defun google-translate-posframe--generic-block-bounds ()
  "Return (START . END) of the paragraph at point via paragraph movement."
  (let ((end (save-excursion (forward-paragraph 1) (point)))
        (start (save-excursion
                 (forward-paragraph 1)
                 (backward-paragraph 1)
                 (skip-chars-forward " \t\n")
                 (point))))
    (cons start end)))

(defun google-translate-posframe--org-block-bounds ()
  "Return (START . END) of the Org paragraph or list item at point.
List bullets and checkboxes are excluded from the returned bounds."
  (let* ((el (org-element-at-point))
         (type (org-element-type el)))
    ;; On a list marker point resolves to the whole list; step past the
    ;; bullet so the element becomes the item's own paragraph.
    (when (memq type '(plain-list item))
      (save-excursion
        (beginning-of-line)
        (when (looking-at "[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+\\(?:\\[[ Xx-]\\][ \t]+\\)?")
          (goto-char (match-end 0)))
        (setq el (org-element-at-point)
              type (org-element-type el))))
    (when (eq type 'paragraph)
      (let ((cb (org-element-property :contents-begin el))
            (ce (org-element-property :contents-end el)))
        (when (and cb ce) (cons cb ce))))))

(defun google-translate-posframe--markdown-block-bounds ()
  "Return (START . END) of the Markdown paragraph or list item at point.
Leading list bullets, checkboxes and blockquote markers are excluded."
  (let ((bounds (google-translate-posframe--generic-block-bounds)))
    (save-excursion
      (goto-char (car bounds))
      (when (looking-at (concat "[ \t]*\\(?:[-+*]\\|[0-9]+[.)]\\)[ \t]+"
                                "\\(?:\\[[ xX]\\][ \t]+\\)?"
                                "\\|[ \t]*>[ \t]?"))
        (setcar bounds (match-end 0))))
    bounds))

(defun google-translate-posframe--block-bounds ()
  "Return (START . END) of the current text block, major-mode aware.
The block is the current paragraph or list item with structural markers
excluded.  Returns nil when no block is found."
  (cond
   ((and (derived-mode-p 'org-mode) (fboundp 'org-element-at-point))
    (google-translate-posframe--org-block-bounds))
   ((derived-mode-p 'markdown-mode)
    (google-translate-posframe--markdown-block-bounds))
   (t
    (google-translate-posframe--generic-block-bounds))))

(defun google-translate-posframe--at-paragraph-boundary-p ()
  "Return non-nil if point is within 5 characters of the current block edge."
  (when-let* ((bounds (google-translate-posframe--block-bounds)))
    (let ((threshold 5))
      (or (<= (abs (- (point) (car bounds))) threshold)
          (<= (abs (- (point) (cdr bounds))) threshold)))))

(defun google-translate-posframe--get-paragraph-text ()
  "Get the current paragraph or list item text, truncated if too long."
  (when-let* ((bounds (google-translate-posframe--block-bounds))
              (text (string-trim (buffer-substring-no-properties
                                  (car bounds) (cdr bounds)))))
    (unless (string-empty-p text)
      (google-translate-posframe--truncate-to-max-length text))))

(defun google-translate-posframe--at-sentence-start-p (&optional min-pos)
  "Return non-nil when point is at the beginning of a sentence.
The preceding sentence terminator may be separated by whitespace and at
most one line break, so sentences that begin on a wrapped line are still
recognized.  MIN-POS bounds how far back the terminator may be."
  (and (looking-at "[[:upper:][:digit:]]")
       (< (or min-pos (point-min)) (point))
       (save-excursion
         (skip-chars-backward " \t")
         (when (eq (char-before) ?\n)
           (backward-char)
           (skip-chars-backward " \t"))
         (and (or (null min-pos) (<= min-pos (point)))
              (memq (char-before) '(?. ?? ?!))))))

(defun google-translate-posframe--get-sentence-text ()
  "Return the sentence starting at point, bounded by the current block.
Returns nil when point is not at a sentence start."
  (when-let* ((bounds (google-translate-posframe--block-bounds)))
    (when (google-translate-posframe--at-sentence-start-p (car bounds))
      (save-excursion
        (let* ((start (point))
               (end (min (cdr bounds) (progn (forward-sentence) (point))))
               (text (string-trim (buffer-substring-no-properties start end))))
          (unless (string-empty-p text)
            (google-translate-posframe--truncate-to-max-length text)))))))

(defun google-translate-posframe--get-text-to-translate ()
  "Get text to translate based on context.
Priority: active region > heading > paragraph or list item (if at its
boundary) > sentence (if at its start) > word at point.  Detection is
major-mode aware for Org and Markdown.  Returns a string or nil."
  (cond
   ;; Active region
   ((use-region-p)
    (string-trim
     (buffer-substring-no-properties (region-beginning) (region-end))))
   ;; Heading line (Org/Markdown)
   ((google-translate-posframe--heading-text))
   ;; Paragraph or list item if at boundary
   ((google-translate-posframe--at-paragraph-boundary-p)
    (google-translate-posframe--get-paragraph-text))
   ;; Sentence if at its beginning
   ((google-translate-posframe--get-sentence-text))
   ;; Word at point
   ((word-at-point)
    (substring-no-properties (word-at-point)))
   (t nil)))

;;; Translation fetching

(defun google-translate-posframe--fetch-translation (source-lang target-lang text callback)
  "Fetch translation for TEXT from SOURCE-LANG to TARGET-LANG.
Call CALLBACK with (text translation) when done.
Uses cache when available."
  (if-let ((cached (google-translate-posframe--cache-get source-lang target-lang text)))
      (funcall callback text cached)
    (condition-case err
        (when-let* ((json (google-translate-request source-lang target-lang text))
                    (translation (google-translate-json-translation json)))
          (google-translate-posframe--cache-put source-lang target-lang text translation)
          (funcall callback text translation))
      (error
       (message "Google Translate posframe error: %s" (error-message-string err))))))

;;; Popup display

(defun google-translate-posframe--format-translation (_text translation source-lang target-lang)
  "Format TRANSLATION with a footer showing SOURCE-LANG -> TARGET-LANG."
  (let* ((direction-footer
          (propertize
           (format "\n\n%s → %s" source-lang target-lang)
           'face 'google-translate-posframe-footer-face))
         (full-text (concat translation direction-footer)))
    (with-temp-buffer
      (insert full-text)
      (let ((fill-column google-translate-posframe-max-width))
        (fill-region (point-min) (point-max)))
      (buffer-string))))

(defun google-translate-posframe--hide ()
  "Hide the translation posframe popup."
  (when google-translate-posframe--available-p
    (posframe-hide google-translate-posframe--buffer))
  (setq google-translate-posframe--last-text nil)
  (google-translate-posframe--remove-oneshot-hooks))

(defun google-translate-posframe--cancel-pending ()
  "Cancel any pending translation fetch timer."
  (when google-translate-posframe--pending-timer
    (cancel-timer google-translate-posframe--pending-timer)
    (setq google-translate-posframe--pending-timer nil)))

(defun google-translate-posframe--oneshot-dismiss ()
  "One-shot hook to auto-dismiss popup on the next command.
Skips the first invocation (the command that triggered the popup)."
  (if (eq google-translate-posframe--oneshot-hooks-active 'first-run)
      (setq google-translate-posframe--oneshot-hooks-active t)
    (google-translate-posframe--hide)))

(defun google-translate-posframe--add-oneshot-hooks ()
  "Add one-shot hooks to dismiss popup on next user action.
Used for explicit commands (not the minor mode)."
  (unless (or google-translate-posframe-mode
              google-translate-posframe--oneshot-hooks-active)
    (setq google-translate-posframe--oneshot-hooks-active 'first-run)
    (add-hook 'post-command-hook #'google-translate-posframe--oneshot-dismiss nil t)
    (when (bound-and-true-p evil-mode)
      (add-hook 'evil-insert-state-entry-hook #'google-translate-posframe--oneshot-dismiss nil t))))

(defun google-translate-posframe--remove-oneshot-hooks ()
  "Remove one-shot auto-dismiss hooks."
  (when google-translate-posframe--oneshot-hooks-active
    (setq google-translate-posframe--oneshot-hooks-active nil)
    (remove-hook 'post-command-hook #'google-translate-posframe--oneshot-dismiss t)
    (when (bound-and-true-p evil-mode)
      (remove-hook 'evil-insert-state-entry-hook #'google-translate-posframe--oneshot-dismiss t))))

(defun google-translate-posframe--maybe-hide ()
  "Hide popup if the current buffer is not the source buffer anymore."
  (when (and google-translate-posframe--available-p
             (posframe-workable-p)
             (get-buffer google-translate-posframe--buffer))
    (let* ((popup-buf (get-buffer google-translate-posframe--buffer))
           (frame (buffer-local-value 'posframe--frame popup-buf)))
      (when (and frame (frame-live-p frame) (frame-visible-p frame))
        (let ((source-buf (buffer-local-value
                           'google-translate-posframe--source-buffer popup-buf)))
          (when (or (not (eq (current-buffer) source-buf))
                    (not (get-buffer-window source-buf)))
            (google-translate-posframe--hide)))))))

(defun google-translate-posframe--show (text translation source-lang target-lang)
  "Show TRANSLATION for TEXT in a posframe popup.
SOURCE-LANG and TARGET-LANG are displayed in the footer."
  (when google-translate-posframe--available-p
    (let ((formatted-text (google-translate-posframe--format-translation
                           text translation source-lang target-lang))
          (current-buf (current-buffer)))
      (posframe-show
       google-translate-posframe--buffer
       :string formatted-text
       :position (point)
       :poshandler google-translate-posframe-poshandler
       :border-width 1
       :border-color (face-foreground 'google-translate-posframe-border-face nil t)
       :background-color (face-background 'tooltip nil t)
       :foreground-color (face-foreground 'tooltip nil t)
       :internal-border-width 12
       :internal-border-color (face-background 'tooltip nil t)
       :left-fringe 8
       :right-fringe 8
       :override-parameters '((no-accept-focus . t)))
      (with-current-buffer google-translate-posframe--buffer
        (setq-local google-translate-posframe--source-buffer current-buf)))))

(defun google-translate-posframe--update ()
  "Update translation popup for text at point.
Called from `post-command-hook' when the minor mode is active."
  (if-let* ((text (google-translate-posframe--get-text-to-translate))
            (_ (or (not (bound-and-true-p evil-mode))
                   (evil-normal-state-p)
                   (evil-visual-state-p)))
            (source-lang google-translate-default-source-language)
            (target-lang google-translate-default-target-language))
      (progn
        (if (equal text google-translate-posframe--last-text)
            nil ; same text, keep showing
          (google-translate-posframe--cancel-pending)
          (setq google-translate-posframe--last-text text)
          (if-let ((translation (google-translate-posframe--cache-get
                                 source-lang target-lang text)))
              (google-translate-posframe--show text translation source-lang target-lang)
            (let ((current-buf (current-buffer))
                  (current-pos (point)))
              (setq google-translate-posframe--pending-timer
                    (run-with-idle-timer
                     google-translate-posframe-idle-delay nil
                     (lambda ()
                       (when (and (buffer-live-p current-buf)
                                  (eq current-buf (current-buffer))
                                  (= current-pos (point)))
                         (google-translate-posframe--fetch-translation
                          source-lang target-lang text
                          (lambda (txt trans)
                            (when (and (buffer-live-p current-buf)
                                       (eq current-buf (current-buffer))
                                       (equal txt (google-translate-posframe--get-text-to-translate)))
                              (google-translate-posframe--show
                               txt trans source-lang target-lang))))))))))))
    (google-translate-posframe--cancel-pending)
    (google-translate-posframe--hide)))

;;; Public API

;;;###autoload
(define-minor-mode google-translate-posframe-mode
  "Minor mode showing Google Translate translations in a posframe popup.
Automatically translates text at point based on context:
- Active region (if selected)
- Heading title (on an Org or Markdown heading line)
- Current paragraph or list item (if at beginning or end)
- Current sentence (if at beginning)
- Word at point (fallback)

Detection is major-mode aware: in Org and Markdown buffers headings,
list items and their sentences are recognized, with structural markers
such as stars, bullets and checkboxes excluded from the translated text.

Uses `google-translate-default-source-language' and
`google-translate-default-target-language' for translation direction."
  :global nil
  :lighter " GTp"
  (if google-translate-posframe-mode
      (progn
        (unless google-translate-posframe--available-p
          (message "google-translate-posframe-mode requires posframe in GUI Emacs")
          (setq google-translate-posframe-mode nil))
        (when google-translate-posframe-mode
          (add-hook 'post-command-hook #'google-translate-posframe--update nil t)
          (when (bound-and-true-p evil-mode)
            (add-hook 'evil-insert-state-entry-hook
                      #'google-translate-posframe--hide nil t))
          (add-hook 'buffer-list-update-hook #'google-translate-posframe--maybe-hide)))
    (remove-hook 'post-command-hook #'google-translate-posframe--update t)
    (when (bound-and-true-p evil-mode)
      (remove-hook 'evil-insert-state-entry-hook
                    #'google-translate-posframe--hide t))
    (remove-hook 'buffer-list-update-hook #'google-translate-posframe--maybe-hide)
    (google-translate-posframe--cancel-pending)
    (google-translate-posframe--hide)))

;;;###autoload
(defun google-translate-posframe-at-point ()
  "Show translation popup for text at point.
Works without `google-translate-posframe-mode'.
Translates region, paragraph, sentence, or word depending on context.
Auto-dismisses on next command."
  (interactive)
  (when-let* ((text (google-translate-posframe--get-text-to-translate))
              (source-lang google-translate-default-source-language)
              (target-lang google-translate-default-target-language))
    (google-translate-posframe--fetch-translation
     source-lang target-lang text
     (lambda (txt trans)
       (google-translate-posframe--show txt trans source-lang target-lang)
       (google-translate-posframe--add-oneshot-hooks)))))

;;;###autoload
(defun google-translate-posframe-region (start end)
  "Show translation popup for region between START and END.
Auto-dismisses on next command."
  (interactive "r")
  (let* ((text (string-trim (buffer-substring-no-properties start end)))
         (source-lang google-translate-default-source-language)
         (target-lang google-translate-default-target-language))
    (when (and source-lang target-lang (not (string-empty-p text)))
      (google-translate-posframe--fetch-translation
       source-lang target-lang text
       (lambda (txt trans)
         (google-translate-posframe--show txt trans source-lang target-lang)
         (google-translate-posframe--add-oneshot-hooks))))))

;;;###autoload
(defun google-translate-posframe-paragraph ()
  "Show translation popup for the current paragraph.
Auto-dismisses on next command."
  (interactive)
  (when-let* ((text (google-translate-posframe--get-paragraph-text))
              (source-lang google-translate-default-source-language)
              (target-lang google-translate-default-target-language))
    (google-translate-posframe--fetch-translation
     source-lang target-lang text
     (lambda (txt trans)
       (google-translate-posframe--show txt trans source-lang target-lang)
       (google-translate-posframe--add-oneshot-hooks)))))

;;;###autoload
(defun google-translate-posframe-word ()
  "Show translation popup for the word at point.
Auto-dismisses on next command."
  (interactive)
  (when-let* ((text (and (word-at-point)
                         (substring-no-properties (word-at-point))))
              (source-lang google-translate-default-source-language)
              (target-lang google-translate-default-target-language))
    (google-translate-posframe--fetch-translation
     source-lang target-lang text
     (lambda (txt trans)
       (google-translate-posframe--show txt trans source-lang target-lang)
       (google-translate-posframe--add-oneshot-hooks)))))

;;;###autoload
(defun google-translate-posframe-dismiss ()
  "Dismiss the translation posframe popup."
  (interactive)
  (google-translate-posframe--hide))

(provide 'google-translate-posframe)
;;; google-translate-posframe.el ends here


;; Local Variables:
;; package-lint-main-file: "google-translate.el"
;; End:
