;;; google-translate-cache.el --- Caching translation results

;; Copyright (C) 2012 Oleksandr Manzyuk <manzyuk@gmail.com>

;; Author: Oleksandr Manzyuk <manzyuk@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/google-translate
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

;; Caches translation results.

;; Segmentation into multiple files is used, so that when the cache
;; is needed, there's no need to load everything.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'recentf)


(defvar google-translate-cache-files nil)


(defgroup google-translate-cache nil
  "Google Translate cache script."
  :group 'processes)


(defcustom google-translate-use-cache t
  "Whether the caching is on.

Only strings containing less words than `google-translate-cache-word-limit'
are cached.  These strings also get downcased beforehand if
`google-translate-downcase-cached-text' is non-nil."
  :type '(boolean)
  :group 'google-translate-cache)

(defcustom google-translate-cache-directory "~/.emacs.d/var/translate-cache"
  "Where the cache is kept."
  :type '(string)
  :group 'google-translate-cache)

(defcustom google-translate-cache-downcase-requests t
  "Set to non-nil to downcase translation requests.

This helps to cache words which are in the beginning of a sentence."
  :type '(boolean)
  :group 'google-translate-cache)

(defcustom google-translate-cache-files-per-language 101
  "How many files to keep cache in (per language pair).

Cache may get quite large and keeping it all in many files has the advantage of
not having to load/save all of it at once, but rather in chunks.  This variable
is how many chunks there will be (at most).  Cache will be automatically rebuilt
once you change this variable, on first cache access."
  :type '(integer)
  :group 'google-translate-cache)

(defcustom google-translate-cache-word-limit 2
  "Don't cache strings which have more words than this.
The number of words is deduced from `split-string'.
Set to nil for no limit."
  :type '(integer)
  :group 'google-translate-cache)

(defmacro google-translate--cache-preserve-recentf (&rest body)
  "Save a copy of `recentf-list' before executing BODY and then restore it.

Because cache files don't need to be in recentf."
  (let ((recentf-restore (gensym))
        (res (gensym)))
    `(let ((,recentf-restore (copy-sequence recentf-list))
           (,res (progn ,@body)))
       (setf recentf-list ,recentf-restore)
       ,res)))

(defun google-translate--cache-read-file (path)
  "`Read' file located at PATH.  Return nil if the file doesn't exist."
  (google-translate--cache-preserve-recentf
   (if (file-exists-p path)
       (cadr (let* ((buf (find-file-noselect path))
                    (contents (read buf)))
               (kill-buffer buf)
               contents)))))

(defun google-translate--cache-expand-rpath (rpath)
  "Expand RPATH as path relative to `google-translate-cache-directory'."
  (expand-file-name
   (concat google-translate-cache-directory
           (unless (equal (substring google-translate-cache-directory -1) "/") "/")
           rpath)))

(defun google-translate--cache-get-file (rpath)
  "Load file `google-translate-cache-directory'/RPATH.

Return it as a property list, :dirty as nil and :contents as the contents of
the file.  :dirty being non-nil indicates the need to be saved by
`google-translate-cache-save'."
  (let ((path (google-translate--cache-expand-rpath rpath)))
    (cl-flet ((get () (alist-get path google-translate-cache-files nil nil 'equal)))
      (or (get)
          (progn
            (push (cons path (list :dirty nil :contents (google-translate--cache-read-file path)))
                  google-translate-cache-files)
            (get))))))

(defun google-translate--cache-rebuild-helper (source-language target-language cached-max)
  "Rebuild cache when `google-translate-cache-files-per-language' doesn't match\
CACHED-MAX found in SOURCE-LANGUAGE/TARGET-LANGUAGE/max-files."
  (message "Rebuilding Google Translate cache...")
  (let ((contents nil)
        (x 0))
    ;; clean out the existing files collecting the cache into `contents'
    (cl-loop for x from 0 to cached-max
             do (let* ((name (concat source-language "/" target-language "/cache-" (int-to-string x)))
                       (segment (google-translate--cache-get-file name)))
                  (setf contents (append contents (plist-get segment :contents)))
                  (delete-file (google-translate--cache-expand-rpath name))
                  (setf google-translate-cache-files
                        (assoc-delete-all (google-translate--cache-expand-rpath name)
                                          google-translate-cache-files))))
    ;; rebuild cache from `contents'
    (dolist (x contents)
      (google-translate--cache-add-helper source-language target-language (car x) (cdr x)))))

(defun google-translate--cache-rebuild-if-necessary (source-language target-language)
  "Recache from SOURCE-LANGUAGE to TARGET-LANGUAGE to fit `google-translate-cache-files-per-language'."
  (let* ((max-files (google-translate--cache-get-file
                     (concat source-language "/" target-language "/max-files")))
         (cached-max (plist-get max-files :contents))
         (needed-max google-translate-cache-files-per-language))
    (if (null cached-max)
        (progn (plist-put max-files :contents needed-max)
               (plist-put max-files :dirty t))
      (when (not (equal cached-max needed-max))
        (google-translate--cache-rebuild-helper source-language target-language cached-max)
        (plist-put max-files :contents google-translate-cache-files-per-language)
        (plist-put max-files :dirty t)
        (google-translate-cache-save)))))

(defun google-translate--cache-get-hash (key)
  "Return cache file # where to keep string KEY."
  ;; (mod (seq-reduce '+ key 0) google-translate-cache-files-per-language)
  ;; testing on ~4000 cached English words, the following produces a more
  ;; uniform distribution in comparison to the simple summation above
  ;; (28--440kb -> 116--256kb per file across 101 files)
  (cl-loop for c across key
           for i from 0 to (length key)
           with s = 0
           do (setf s (mod (+ s (* c (1+ (mod i 127))))
                           google-translate-cache-files-per-language))
           finally return s))

(defun google-translate--cache-get-segment (source-language target-language key)
  "Return file which should contain KEY (from SOURCE-LANGUAGE to TARGET-LANGUAGE)."
  (google-translate--cache-get-file
   (concat source-language
           "/"
           target-language
           "/cache-"
           (int-to-string (google-translate--cache-get-hash key)))))

(defun google-translate--cache-add-helper (source-language target-language key translation)
  "Cache TRANSLATION for KEY from SOURCE-LANGUAGE to TARGET-LANGUAGE.

`google-translate--cache-rebuild-helper' is calling this functions instead of
`google-translate-cache-add', because that one calls
`google-translate--cache-rebuild-if-necessary'."
  (let ((cache-segment
         (google-translate--cache-get-segment source-language target-language key)))
    ;; remove duplicates
    (plist-put cache-segment :contents
               (assoc-delete-all key (plist-get cache-segment :contents)))
    ;; push (KEY . TRANSLATION)
    (plist-put cache-segment :contents
               (cons (cons key translation)
                     (plist-get cache-segment :contents)))
    ;; mark segment as modified
    (plist-put cache-segment :dirty t)))

(defun google-translate-cache-add (source-language target-language key translation)
  "Cache TRANSLATION for KEY from SOURCE-LANGUAGE to TARGET-LANGUAGE.

`google-translate--key-to-segment-id' decides to which file the KEY goes to."
  (google-translate--cache-rebuild-if-necessary source-language target-language)
  (google-translate--cache-add-helper source-language target-language key translation)
  translation)

(defun google-translate-cache-get (source-language target-language key)
  "Get cached translation for KEY from SOURCE-LANGUAGE to TARGET-LANGUAGE."
  (google-translate--cache-rebuild-if-necessary source-language target-language)
  (let ((cache-segment
         (google-translate--cache-get-segment source-language target-language key)))
    (alist-get key (plist-get cache-segment :contents) nil nil 'equal)))

(defun google-translate-cache-save ()
  "Save cache to disk."
  (google-translate--cache-preserve-recentf
   (cl-loop for f in google-translate-cache-files
            do (save-excursion
                 (if (plist-get (cdr f) :dirty)
                     (let ((buf (find-file-noselect (car f))))
                       (plist-put (cdr f) :dirty nil)
                       (set-buffer buf)
                       (erase-buffer)
                       (insert "'")
                       (insert (prin1-to-string (plist-get (cdr f) :contents)))
                       (save-buffer)
                       (kill-buffer)))))))

(add-hook 'kill-emacs-hook 'google-translate-cache-save)

(provide 'google-translate-cache)

;;; google-translate-cache.el ends here
