;;; recursion-indicator.el --- Recursion indicator -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023 Daniel Mendler

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Created: 2020
;; Version: 0.4
;; Package-Requires: ((emacs "27.1") (compat "29.1.3.4"))
;; Homepage: https://github.com/minad/recursion-indicator
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Recursion indicator for the mode line

;;; Code:

(require 'compat)

(defgroup recursion-indicator nil
  "Recursion indicator for the mode line."
  :link '(url-link :tag "Homepage" "https://github.com/minad/recursion-indicator")
  :link '(emacs-library-link :tag "Library Source" "recursion-indicator.el")
  :group 'convenience
  :prefix "recursion-indicator-")

(defface recursion-indicator-completion
  '((t :inherit 'font-lock-variable-face))
  "Face used for the arrow indicating completion recursion.")

(defface recursion-indicator-prompt
  '((t :inherit 'font-lock-keyword-face))
  "Face used for the arrow indicating minibuffer recursion.")

(defface recursion-indicator-general
  '((t :inherit 'font-lock-constant-face))
  "Face used for the arrow indicating general recursion.")

(defcustom recursion-indicator-general "R"
  "Recursion indicator."
  :type 'string)

(defcustom recursion-indicator-completion "C"
  "Minibuffer completion indicator."
  :type 'string)

(defcustom recursion-indicator-prompt "P"
  "Minibuffer prompt indicator."
  :type 'string)

(defvar recursion-indicator--minibuffers nil
  "Minibuffer depth and command alist.")

(defvar recursion-indicator--cache nil
  "Cached recursion indicator.")

(defun recursion-indicator--string ()
  "Recursion indicator string."
  (let* ((str nil)
         (depth (recursion-depth)))
    (if (eq (car recursion-indicator--cache) depth)
        (cdr recursion-indicator--cache)
      (dotimes (i depth)
        (setq str (concat
                   str
                   (if-let (mb (assq (1+ i) recursion-indicator--minibuffers))
                       (if (buffer-local-value 'minibuffer-completion-table (caddr mb))
                           (propertize recursion-indicator-completion
                                       'face 'recursion-indicator-completion
                                       'help-echo (format "%s: Completion `%s'" (1+ i) (cadr mb)))
                         (propertize recursion-indicator-prompt
                                     'face 'recursion-indicator-prompt
                                     'help-echo (format "%s: Prompt `%s'" (1+ i) (cadr mb))))
                     (propertize recursion-indicator-general
                                 'face 'recursion-indicator-general
                                 'help-echo (format "%s: Recursion" (1+ i)))))))
      (when str (setq str (format
                           (propertize " [%s] "
                                       'help-echo (format "Recursion depth %s" depth)
                                       'pointer 'arrow)
                           str)))
      (setq recursion-indicator--cache (cons depth str))
      str)))

(defun recursion-indicator--mb-setup ()
  "Minibuffer setup hook."
  (push (list (recursion-depth)
              (if (and this-command (symbolp this-command))
                  this-command 'unknown)
              (current-buffer))
        recursion-indicator--minibuffers)
  (setq recursion-indicator--cache nil)
  (run-at-time 0 nil #'force-mode-line-update 'all))

(defun recursion-indicator--mb-exit ()
  "Minibuffer exit hook."
  (pop recursion-indicator--minibuffers)
  (setq recursion-indicator--cache nil))

(defun recursion-indicator-exit (arg)
  "Enter recursive edit if prefix ARG is non-nil, otherwise exit.
When entering a new recursive editing session the window
configuration will be saved.  It will be restored the as soon as
the recursive editing session is left."
  (interactive "P")
  (let ((depth (recursion-depth)))
    (cond
     (arg
      (message "Recursion depth: %s" (1+ depth))
      (save-window-excursion (recursive-edit)))
     ((assq depth recursion-indicator--minibuffers)
      (abort-recursive-edit))
     (t
      (when (> depth 0)
        (message "Recursion depth: %s" (1- depth)))
      (exit-recursive-edit)))))

(defvar-keymap recursion-indicator-map
  :doc "Global keymap used by `recursion-indicator-mode'."
  "<remap> <abort-recursive-edit>" #'recursion-indicator-exit
  "<remap> <exit-recursive-edit>" #'recursion-indicator-exit)

;;;###autoload
(define-minor-mode recursion-indicator-mode
  "Show the recursion depth in the mode-line."
  :keymap recursion-indicator-map
  :global t
  (setq recursion-indicator--cache nil)
  (cond
   (recursion-indicator-mode
    ;; Use a high priority such that the indicator reflects the minibuffer recursion
    ;; status even if Emacs is redisplayed in another minibuffer setup hook.
    (add-hook 'minibuffer-setup-hook #'recursion-indicator--mb-setup -99)
    (add-hook 'minibuffer-exit-hook #'recursion-indicator--mb-exit)
    (setf (alist-get 'recursion-indicator-mode mode-line-misc-info)
          '((:eval (recursion-indicator--string)))))
   (t
    (remove-hook 'minibuffer-setup-hook #'recursion-indicator--mb-setup)
    (remove-hook 'minibuffer-exit-hook #'recursion-indicator--mb-exit)
    (setf (alist-get 'recursion-indicator-mode mode-line-misc-info nil t) nil))))

(provide 'recursion-indicator)
;;; recursion-indicator.el ends here
