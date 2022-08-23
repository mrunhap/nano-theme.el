;;; nano-theme.el --- A theme split from nano-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022 LiuBo

;; Author: LiuBo <https://github.com/404cn>
;; Created: May 30, 2021
;; Version: 1.0.0
;; Keywords: theme
;; Homepage: https://github.com/404cn/nano-theme.el
;; Package-Requires: ((emacs "28.0.50"))

;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;;; A theme split from nano-emacs.

;;; Code:

(deftheme nano "Theme split from nano-emacs")

(defgroup nano-theme nil
  "Options of nano theme."
  :group 'faces)

(defcustom nano-theme-light/dark 'light
  "Nano theme uses light theme or dark theme?"
  :type 'symbol
  :group 'nano-theme)

(defun nano-theme--light?dark (light dark)
  "Determine using the LIGHT or the DARK color of nano-theme."
  (if (eq nano-theme-light/dark 'light)
      light
    dark))
(defalias '--l?d #'nano-theme--light?dark)

(defun nano-theme-toggle ()
  "Toggle between light/dark nano theme"
  (interactive)
  (if (eq nano-theme-light/dark 'light)
      (setq nano-theme-light/dark 'dark)
    (setq nano-theme-light/dark 'light))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'nano))

(let ((foreground (--l?d "#37474F" "#ECEFF4"))
      (background (--l?d "#FFFFFF" "#2E3440"))
      (highlight  (--l?d "#FAFAFA" "#3B4252"))
      (critical   (--l?d "#FF6F00" "#EBCB8B"))
      (salient    (--l?d "#673AB7" "#81A1C1"))
      (strong     (--l?d "#263238" "#FFFFFF"))
      (popout     (--l?d "#FFAB91" "#D08770"))
      (subtle     (--l?d "#ECEFF1" "#434C5E"))
      (faded      (--l?d "#90A4AE" "#677691")))
  (custom-theme-set-faces
   `nano
   ;; Basic
   `(default                    ((((type tty)))
                                 (((type graphic)) :background ,background :foreground ,foreground)))
   `(shadow                     ((t (:foreground ,faded))))
   `(link                       ((t (:foreground ,salient))))
   `(link-visited               ((t (:foreground ,salient))))
   `(highlight                  ((t (:background ,highlight))))
   `(match                      ((t (:foreground ,popout))))
   `(isearch                    ((t (:background ,subtle :box (:line-width (-1 . -1))))))
   `(lazy-highlight             ((t (:background ,subtle :box (:line-width (-1 . -1))))))
   `(warning                    ((t (:foreground ,popout))))
   `(success                    ((t (:foreground ,faded))))
   `(cursor                     ((t (:background ,foreground))))
   `(fringe                     ((t (:foreground ,faded))))
   `(show-paren-match           ((t (:box (:line-width (-1 . -1))))))
   `(hl-line                    ((t (:background ,highlight))))
   `(region                     ((t (:background ,subtle))))
   `(line-number                ((t (:foreground ,faded))))
   `(line-number-current-line   ((t (:foreground ,strong))))
   `(minibuffer-prompt          ((t (:foreground ,strong))))
   `(vertical-border            ((t (:foreground ,subtle))))
   `(window-divider             ((t (:foreground ,subtle))))
   `(window-divider-first-pixel ((t (:foreground ,subtle))))
   `(window-divider-last-pixel  ((t (:foreground ,subtle))))
   `(fill-column-indicator      ((t (:foreground ,strong))))
   `(trailing-whitespace        ((t (:background ,subtle))))
   `(completions-common-part    ((t (:foreground ,faded))))
   `(secondary-selection        ((t (:background ,subtle))))
   `(header-line                ((t ( :background ,subtle :foreground ,strong
                                      :box (:line-width 2 :style released-button)))))

   ;; Font Locks
   `(font-lock-comment-face           ((t (:foreground ,faded))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,faded))))
   `(font-lock-keyword-face           ((t (:foreground ,salient))))
   `(font-lock-string-face            ((t (:foreground ,popout))))
   `(font-lock-doc-face               ((t (:foreground ,faded))))
   `(font-lock-builtin-face           ((t ())))
   `(font-lock-type-face              ((t ())))
   `(font-lock-variable-name-face     ((t ())))
   `(font-lock-constant-face          ((t (:foreground ,salient))))
   `(font-lock-function-name-face     ((t (:foreground ,strong :underline t))))
   `(font-lock-warning-face           ((t ())))
   `(font-lock-preprocessor-face      ((t ())))

   ;; Eldoc
   `(eldoc-highlight-function-argument ((t (:foreground ,strong :bold t))))

   ;; Outline
   `(outline-1 ((t (:foreground ,foreground :bold t :slant italic))))
   `(outline-2 ((t (:inherit outline-1))))
   `(outline-3 ((t (:inherit outline-1))))
   `(outline-4 ((t (:inherit outline-1))))
   `(outline-5 ((t (:inherit outline-1))))
   `(outline-6 ((t (:inherit outline-1))))
   `(outline-7 ((t (:inherit outline-1))))
   `(outline-8 ((t (:inherit outline-1))))

   ;; magit
   `(magit-diff-hunk-heading ((t (:background ,subtle))))

   ;; Agenda
   `(org-agenda-calendar-event   ((t (:foreground ,foreground :background ,background))))
   `(org-agenda-calendar-sexp    ((t (:foreground ,salient))))
   `(org-agenda-clocking         ((t (:foreground ,faded))))
   `(org-agenda-column-dateline  ((t (:foreground ,faded))))
   `(org-agenda-current-time     ((t (:foreground ,foreground :bold t))))
   `(org-agenda-date             ((t (:foreground ,salient))))
   `(org-agenda-date-today       ((t (:foreground ,salient :bold t))))
   `(org-agenda-date-weekend     ((t (:foreground ,faded))))
   `(org-agenda-diary            ((t (:foreground ,faded))))
   `(org-agenda-dimmed-todo-face ((t (:foreground ,faded))))
   `(org-agenda-done             ((t (:foreground ,faded))))
   `(org-agenda-filter-category  ((t (:foreground ,faded))))
   `(org-agenda-filter-effort    ((t (:foreground ,faded))))
   `(org-agenda-filter-regexp    ((t (:foreground ,faded))))
   `(org-agenda-filter-tags      ((t (:foreground ,faded))))
   `(org-agenda-restriction-lock ((t (:foreground ,faded))))
   `(org-agenda-structure        ((t (:foreground ,foreground :bold t))))

   ;; Notmuch
   `(notmuch-tag-face             ((t (:foreground ,faded))))
   `(notmuch-search-date          ((t (:foreground ,faded))))
   `(notmuch-tag-deleted          ((t (:strike-through ,popout))))
   `(notmuch-tag-added            ((t (:underline ,popout))))
   `(notmuch-wash-cited-text      ((t (:foreground ,faded))))
   `(notmuch-message-summary-face ((t (:foreground ,strong :bold t :background ,subtle))))

   ;; Mode Line
   `(mode-line          ((t ( :background ,foreground :foreground ,background))))
   `(mode-line-inactive ((t ( :background ,faded :foreground ,background))))

   ;; tab-bar
   `(tab-bar                    ((t (:background ,background :foreground ,foreground))))
   `(tab-bar-tab-group-current  ((t ())))
   `(tab-bar-tab                ((t (:inverse-video t :bold t))))
   `(tab-bar-tab-group-inactive ((t ())))
   `(tab-bar-tab-inactive       ((t (:inherit shadow))))

   ;; solaire Mode
   `(solaire-default-face             ((t (:inherit default :background ,highlight))))

   ;; Orderless
   `(orderless-match-face-0           ((t (:foreground ,strong :bold t))))
   `(orderless-match-face-1           ((t (:foreground ,strong :bold t))))
   `(orderless-match-face-2           ((t (:foreground ,strong :bold t))))
   `(orderless-match-face-3           ((t (:foreground ,strong :bold t))))

   ;; Eshell
   `(eshell-prompt                    ((t (:foreground ,popout :bold t))))

   ;; telega
   `(telega-msg-inline-reply ((t (:foreground ,faded))))
   `(telega-msg-heading      ((t (:underline t))))

   ;; which-func
   `(which-func ((t (:foreground ,highlight))))

   ;; Imenu-ist
   `(imenu-list-entry-subalist-face-0 ((t (:foreground ,strong :weight bold :underline t))))
   `(imenu-list-entry-subalist-face-1 ((t (:foreground ,salient :weight bold :underline t))))
   `(imenu-list-entry-subalist-face-2 ((t (:foreground ,popout :weight bold :underline t))))
   `(imenu-list-entry-subalist-face-3 ((t (:foreground ,critical :weight bold :underline t))))
   `(imenu-list-entry-face-0          ((t (:foreground ,strong))))
   `(imenu-list-entry-face-1          ((t (:foreground ,salient))))
   `(imenu-list-entry-face-2          ((t (:foreground ,popout))))
   `(imenu-list-entry-face-3          ((t (:foreground ,critical))))))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'nano)

;;; nano-theme.el ends here
