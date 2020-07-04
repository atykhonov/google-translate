;;; google-translate-cache-ui.el --- Caching translation results UI

;; Copyright (C) 2012 Oleksandr Manzyuk <manzyuk@gmail.com>

;; Author: Oleksandr Manzyuk <manzyuk@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/google-translate
;; Package-Requires: ((emacs "24.3") (popup "0.5.8"))
;; Version: 0.12.1
;; Keywords: convenience

;; Contributors:
;;   Tassilo Horn <tsdh@gnu.org>
;;   Bernard Hurley <bernard@marcade.biz>
;;   Chris Bilson <cbilson@pobox.com>
;;   Takumi Kinjo <takumi.kinjo@gmail.com>
;;   momomo5717 <momomo5717@gmail.com>
;;   stardiviner <numbchild@gmail.com>
;;   Dmitrii Korobeinikov <dim1212k@gmail.com>

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

;; UI for caching.
;;
;; `google-translate-cache-words-in-buffer' - retreives and caches
;;  translations for every word in the buffer.
;;
;; `google-translate-cache-words-in-region' - retreives and caches
;;  translations for every word in the active region.
;;

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'google-translate-default-ui)

(defun %google-translate-cache-words-in-region (start end &optional override-p)
  "Translate and cache words between START and END.

For the meaning of OVERRIDE-P, see `google-translate-query-translate'."
  (let ((langs (google-translate-read-args override-p nil))
        (word 1)
        (total (count-words start end)))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (forward-word-strictly 1)
          (message "Processing word %d/%d." word total)
          (save-excursion
            (google-translate-translate
             (car langs)
             (cadr langs)
             (let ((b (bounds-of-thing-at-point 'word)))
               (buffer-substring-no-properties (car b) (cdr b)))))
          (cl-incf word))))))


;;;###autoload
(defun google-translate-cache-words-in-region (beg end &optional override-p)
  "Cache translations for all words in active region (from BEG to END).

For the meaning of OVERRIDE-P, see `google-translate-query-translate'."
  (interactive "rP")
  (%google-translate-cache-words-in-region beg end override-p))

;;;###autoload
(defun google-translate-cache-words-in-buffer (&optional override-p)
  "Cache translations for all words in current buffer.

For the meaning of OVERRIDE-P, see `google-translate-query-translate'."
  (interactive "P")
  (%google-translate-cache-words-in-region (buffer-end -1) (buffer-end 1) override-p))

(provide 'google-translate-cache-ui)

;;; google-translate-cache-ui.el ends here
