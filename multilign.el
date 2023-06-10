;; multilign.el --- Aligne thyne multílynne ſtryngges -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Yu Han Quek

;; Author: Yu Han Quek <yuhan0@users.noreply.github.com>
;; Keywords: faces, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A tiny minor mode for displaying multiline string literals with padded
;; indents, correctly aligning continuation lines to the first line of text
;; (per its output).
;;
;; Does *not* make any modifications to the underlying text, and relies on
;; font-lock-mode's JIT machinery to apply and update itself across visible
;; portions of the buffer.

;;; Usage:

;; Add to your init file:
;; (add-hook 'emacs-lisp-mode-hook #'multilign-mode)
;; (add-hook 'clojure-mode-hook #'multilign-mode) ;; etc.
;;
;; Tested mostly in lisps, and seems to work fine with triple-quoted strings in
;; python/julia-mode but with inconsistent alignment due to differences in how
;; syntax-ppss parses them.
;;
;; TODO: custom offsets per mode?
;; - Might want special cases for docstring contexts
;; - eg. don't pad Python docstrings, which get auto-trimmed by compiler.

;;; Code:

(require 'thingatpt)

(defface multilign-padding-face
  '((t :inherit whitespace-newline))
  "Face used to propertize multiline string paddings."
  :group 'multilign)

(defcustom multilign-display-char ?·
  "Character to display in the padding.
Make sure your fixed-space font supports it!"
  :type '(choice (const :tag "Dots" ?·)
                 (const :tag "Waves" ?~)
                 (const :tag "Grids" ?┼)
                 (const :tag "Stripes" ?│)
                 (const :tag "Hatches" ?\\)
                 (const :tag "Splotches" ?✱)
                 (const :tag "Nothingness" ?\s)
                 character)
  :group 'multilign)

(defcustom multilign-affect-wrapped-lines t
  "Whether to apply padding to `wrap-prefix' of continuation lines.
When non-nil, the line fragments wrapped around when `truncate-lines' is nil
will also be indented to the starting column."
  :type 'boolean
  :group 'multilign)

(defun multilign--propertize (limit)
  "Append line prefixes to multiline strings between point and LIMIT."
  (let (bnds success)
    (unwind-protect
        (progn
          (while (and (< (point) limit)
                      (not (setq bnds (bounds-of-thing-at-point 'string))))
            ;; place point before the next string quote/fence and try again
            (forward-char 1)
            (skip-syntax-forward "^|\"" limit))
          (if (not bnds)
              (goto-char limit)
            (let ((beg (car bnds))
                  (end (cdr bnds)))
              (when (< (line-number-at-pos beg) (line-number-at-pos end))
                ;; Multiline string - skip past the starting quotes
                (goto-char beg)
                (skip-syntax-forward "|\"")
                (let ((padding (propertize
                                (make-string (current-column)
                                             (or multilign-display-char ?\s))
                                'face 'multilign-padding-face)))
                  (put-text-property (point) end 'line-prefix padding)
                  (when multilign-affect-wrapped-lines
                    (put-text-property (point) end 'wrap-prefix padding))))
              (goto-char end)))
          (setq success t))
      (or success ;; Something went wrong! Abort to avoid breaking Emacs entirely
          (multilign-mode -1)
          (user-error "Multilign-mode shut itself down!")))))

(defvar-local multilign--saved-font-lock-props nil
  "Saved value of `font-lock-extra-managed-props' to restore upon leaving the mode.")

(define-minor-mode multilign-mode
  "Visually align multiline strings with line paddings."
  :lighter " ǉn" :keymap nil
  (if multilign-mode
      (progn
        (font-lock-add-keywords nil '((multilign--propertize)))
        (setq-local multilign--saved-font-lock-props font-lock-extra-managed-props)
        (make-local-variable 'font-lock-extra-managed-props)
        (add-to-list 'font-lock-extra-managed-props 'line-prefix)
        (when multilign-affect-wrapped-lines
          (add-to-list 'font-lock-extra-managed-props 'wrap-prefix))
        (when font-lock-mode
          (font-lock-flush)
          (font-lock-ensure)))
    (font-lock-remove-keywords nil '((multilign--propertize)))
    (when font-lock-mode
      (font-lock-flush)
      (font-lock-ensure))
    ;; Restore previous value, don't `kill-local-variable' - other minor modes
    ;; might have also added to it.
    ;; FIXME what if they were loaded aftter and added to it in the meantime?
    ;; Maybe there's no harm leaving the extra managed props hanging around
    (setq font-lock-extra-managed-props multilign--saved-font-lock-props)))

(provide 'multilign)
;;; multilign.el ends here
