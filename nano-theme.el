;;; nano-theme.el --- A theme split from nano-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 LiuBo

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

(defcustom nano-theme-header-scales '(1.3 1.2 1.1 1.0 1.0 1.0 1.0)
  "Scales for headers."
  :group 'nano-theme)

(defcustom nano-theme-light/dark 'light
  "Nano theme uses light theme or dark theme?"
  :type 'symbol
  :group 'nano-theme)

(defcustom nano-theme-padded-modeline 4
  "If non-nil, add a 4px padding to the mode-line. Can be an integer to determine the exact padding"
  :type '(choice integer boolean)
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
      (strong     (--l?d "#000000" "#ECEFF4"))
      (popout     (--l?d "#FFAB91" "#D08770"))
      (subtle     (--l?d "#ECEFF1" "#434C5E"))
      (faded      (--l?d "#B0BEC5" "#677691"))
      (-modeline-pad
       (when nano-theme-padded-modeline
         (if (integerp nano-theme-padded-modeline) nano-theme-padded-modeline 4))))
  (custom-theme-set-faces
   `nano
   ;; Basic
   `(default                    ((t (:foreground ,foreground :background ,background))))
   `(cursor                     ((t (:background ,foreground))))
   `(fringe                     ((t (:foreground ,faded))))
   `(show-paren-match           ((t (:background ,subtle :box (:line-width (-1 . -1))))))
   `(hl-line                    ((t (:background ,highlight))))
   `(highlight                  ((t (:background ,subtle))))
   `(lazy-highlight             ((t (:background ,subtle :box (:line-width (-1 . -1))))))
   `(region                     ((t (:background ,faded))))
   `(line-number                ((t (:background ,highlight :foreground ,faded))))
   `(line-number-current-line   ((t (:background ,highlight :foreground ,strong))))
   `(minibuffer-prompt          ((t (:foreground ,strong))))
   `(vertical-border            ((t (:foreground ,subtle))))
   `(window-divider             ((t (:foreground ,subtle))))
   `(window-divider-first-pixel ((t (:foreground ,subtle))))
   `(window-divider-last-pixel  ((t (:foreground ,subtle))))
   `(fill-column-indicator      ((t (:foreground ,strong))))
   `(shadow                     ((t (:foreground ,faded))))
   `(success                    ((t (:foreground ,faded))))
   `(warning                    ((t (:foreground ,popout))))
   `(error                      ((t (:foreground ,critical))))
   `(match                      ((t (:foreground ,popout))))
   `(link                       ((t (:foreground ,salient))))
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

   ;; ISearch
   `(isearch      ((t (:foreground ,strong))))
   `(isearch-fail ((t (:foreground ,faded))))

   ;; Info
   `(info-menu-header ((t (:foreground ,foreground :bold t))))
   `(info-header-node ((t (:foreground ,foreground :background ,background))))
   `(info-index-match ((t (:foreground ,salient))))
   `(Info-quoted      ((t (:foreground ,faded))))
   `(info-title-1     ((t (:inherit info-menu-header))))
   `(info-title-2     ((t (:inherit info-menu-header))))
   `(info-title-3     ((t (:inherit info-menu-header))))
   `(info-title-4     ((t (:inherit info-menu-header))))

   ;; Bookmark
   `(bookmark-face          ((t (:foreground ,popout :box t))))
   `(bookmark-menu-heading  ((t (:foreground ,foreground :bold t))))
   `(bookmark-menu-bookmark ((t (:foreground ,salient))))

   ;; Outline
   `(outline-1 ((t (:foreground ,foreground :bold t))))
   `(outline-2 ((t (:foreground ,foreground :bold t))))
   `(outline-3 ((t (:foreground ,foreground :bold t))))
   `(outline-4 ((t (:foreground ,foreground :bold t))))
   `(outline-5 ((t (:foreground ,foreground :bold t))))
   `(outline-6 ((t (:foreground ,foreground :bold t))))
   `(outline-7 ((t (:foreground ,foreground :bold t))))
   `(outline-8 ((t (:foreground ,foreground :bold t))))

   ;; Message
   `(message-cited-text        ((t (:foreground ,faded))))
   `(message-header-cc         ((t (:foreground ,foreground :background ,background))))
   `(message-header-name       ((t (:foreground ,foreground :bold t))))
   `(message-header-newsgroups ((t (:foreground ,foreground :background ,background))))
   `(message-header-other      ((t (:foreground ,foreground :background ,background))))
   `(message-header-subject    ((t (:foreground ,salient))))
   `(message-header-to         ((t (:foreground ,salient))))
   `(message-header-xheader    ((t (:foreground ,foreground :background ,background))))
   `(message-mml               ((t (:foreground ,popout))))
   `(message-separator         ((t (:foreground ,faded))))

   ;; Customize
   `(widget-field             ((t (:background ,subtle))))
   `(widget-button            ((t (:foreground ,foreground :bold t))))
   `(widget-single-line-field ((t (:background ,subtle))))
   `(custom-group-subtitle    ((t (:foreground ,foreground :bold t))))
   `(custom-group-tag         ((t (:foreground ,foreground :bold t))))
   `(custom-group-tag-1       ((t (:foreground ,foreground :bold t))))
   `(custom-comment           ((t (:foreground ,faded))))
   `(custom-comment-tag       ((t (:foreground ,faded))))
   `(custom-changed           ((t (:foreground ,salient))))
   `(custom-modified          ((t (:foreground ,salient))))
   `(custom-face-tag          ((t (:foreground ,foreground :bold t))))
   `(custom-variable-tag      ((t (:foreground ,foreground :bold t))))
   `(custom-invalid           ((t (:foreground ,popout))))
   `(custom-visibility        ((t (:foreground ,salient))))
   `(custom-state             ((t (:foreground ,salient))))
   `(custom-link              ((t (:foreground ,salient))))
   `(custom-button            ((t (:foreground ,faded :background ,background :box `(:line-width 1 :color ,(face-foreground 'faded) :style nil)))))
   `(custom-button-mouse      ((t (:foreground ,faded :background ,subtle :box `(:line-width 1 :color ,(face-foreground 'faded) :style nil)))))
   `(custom-button-pressed    ((t (:foreground ,foreground :background ,salient :inverse-video nil :box `(:line-width 1 :color ,(face-foreground 'salient) :style nil)))))

   ;; Package
   `(package-description       ((t (:foreground ,foreground :background ,background))))
   `(package-help-section-name ((t (t (:foreground ,foreground :background ,background)))))
   `(package-name              ((t (:foreground ,salient))))
   `(package-status-avail-obso ((t (:foreground ,faded))))
   `(package-status-available  ((t (:foreground ,foreground :background ,background))))
   `(package-status-built-in   ((t (:foreground ,salient))))
   `(package-status-dependency ((t (t (:foreground ,salient)))))
   `(package-status-disabled   ((t (:foreground ,faded))))
   `(package-status-external   ((t (:foreground ,foreground :background ,background))))
   `(package-status-held       ((t (:foreground ,foreground :background ,background))))
   `(package-status-incompat   ((t (:foreground ,faded))))
   `(package-status-installed  ((t (:foreground ,salient))))
   `(package-status-new        ((t (:foreground ,foreground :background ,background))))
   `(package-status-new        ((t (:foreground ,foreground :background ,background))))

   ;; Flyspell
   `(flyspell-duplicate ((t (:underline ,critical))))
   `(flyspell-incorrect ((t (:underline ,critical))))

   ;; Diff
   `(diff-context     ((t (:foreground ,faded))))
   `(diff-hunk-header ((t (:background ,subtle :foreground ,foreground))))
   `(diff-function    ((t (:inherit diff-hunk-header))))
   `(diff-header      ((t (:foreground ,strong :bold t))))
   `(diff-file-header ((t (:inherit diff-header))))

   ;; magit
   `(magit-diff-hunk-heading ((t (:background ,subtle))))

   ;; dired-hacks
   `(dired-subtree-depth-1-face ((t (:background ,background))))
   `(dired-subtree-depth-2-face ((t (:background ,background))))
   `(dired-subtree-depth-3-face ((t (:background ,background))))
   `(dired-subtree-depth-4-face ((t (:background ,background))))
   `(dired-subtree-depth-5-face ((t (:background ,background))))
   `(dired-subtree-depth-6-face ((t (:background ,background))))

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

   ;; Org
   `(org-archived                 ((t (:foreground ,faded))))
   `(org-block                    ((t (:background ,highlight))))
   `(org-block-begin-line         ((t (:foreground ,faded))))
   `(org-block-end-line           ((t (:foreground ,faded))))
   `(org-checkbox                 ((t (:foreground ,faded))))
   `(org-checkbox-statistics-done ((t (:foreground ,faded))))
   `(org-checkbox-statistics-todo ((t (:foreground ,faded))))
   `(org-clock-overlay            ((t (:foreground ,faded))))
   `(org-code                     ((t (:foreground ,faded))))
   `(org-column                   ((t (:foreground ,faded))))
   `(org-column-title             ((t (:foreground ,faded))))
   `(org-date                     ((t (:foreground ,faded))))
   `(org-date-selected            ((t (:foreground ,faded))))
   `(org-default                  ((t (:foreground ,faded))))
   `(org-document-info            ((t (:foreground ,faded))))
   `(org-document-info-keyword    ((t (:foreground ,faded))))
   `(org-document-title           ((t (:foreground ,faded))))
   `(org-done                     ((t (:foreground ,foreground :background ,background))))
   `(org-drawer                   ((t (:foreground ,faded))))
   `(org-ellipsis                 ((t (:foreground ,faded))))
   `(org-footnote                 ((t (:foreground ,faded))))
   `(org-formula                  ((t (:foreground ,faded))))
   `(org-headline-done            ((t (:foreground ,faded))))
   `(org-latex-and-related        ((t (:foreground ,faded))))
   `(org-level-1                  ((t (:foreground ,foreground :bold t :height ,(nth 0 nano-theme-header-scales)))))
   `(org-level-2                  ((t (:inherit org-level-1 :height ,(nth 1 nano-theme-header-scales)))))
   `(org-level-3                  ((t (:inherit org-level-1 :height ,(nth 2 nano-theme-header-scales)))))
   `(org-level-4                  ((t (:inherit org-level-1 :height ,(nth 3 nano-theme-header-scales)))))
   `(org-level-5                  ((t (:inherit org-level-1 :height ,(nth 4 nano-theme-header-scales)))))
   `(org-level-6                  ((t (:inherit org-level-1 :height ,(nth 5 nano-theme-header-scales)))))
   `(org-level-7                  ((t (:inherit org-level-1 :height ,(nth 6 nano-theme-header-scales)))))
   `(org-link                     ((t (:foreground ,salient))))
   `(org-list-dt                  ((t (:foreground ,faded))))
   `(org-macro                    ((t (:foreground ,faded))))
   `(org-meta-line                ((t (:foreground ,faded))))
   `(org-mode-line-clock          ((t (:foreground ,faded))))
   `(org-mode-line-clock-overrun  ((t (:foreground ,faded))))
   `(org-priority                 ((t (:foreground ,faded))))
   `(org-property-value           ((t (:foreground ,faded))))
   `(org-quote                    ((t (:foreground ,faded))))
   `(org-scheduled                ((t (:foreground ,faded))))
   `(org-scheduled-previously     ((t (:foreground ,faded))))
   `(org-scheduled-today          ((t (:foreground ,faded))))
   `(org-sexp-date                ((t (:foreground ,faded))))
   `(org-special-keyword          ((t (:foreground ,faded))))
   `(org-table                    ((t (:foreground ,faded))))
   `(org-tag                      ((t (:foreground ,popout))))
   `(org-tag-group                ((t (:foreground ,faded))))
   `(org-target                   ((t (:foreground ,faded))))
   `(org-time-grid                ((t (:foreground ,faded))))
   `(org-todo                     ((t (:foreground ,salient))))
   `(org-upcoming-deadline        ((t (:foreground ,foreground :background ,background))))
   `(org-verbatim                 ((t (:foreground ,popout))))
   `(org-verse                    ((t (:foreground ,faded))))
   `(org-warning                  ((t (:foreground ,popout))))

   ;; Elfeed
   `(elfeed-log-date-face            ((t (:foreground ,faded))))
   `(elfeed-log-info-level-face      ((t (:foreground ,foreground :background ,background))))
   `(elfeed-log-debug-level-face     ((t (:foreground ,foreground :background ,background))))
   `(elfeed-log-warn-level-face      ((t (:foreground ,popout))))
   `(elfeed-log-error-level-face     ((t (:foreground ,popout))))
   `(elfeed-search-tag-face          ((t (:foreground ,faded))))
   `(elfeed-search-date-face         ((t (:foreground ,faded))))
   `(elfeed-search-feed-face         ((t (:foreground ,salient))))
   `(elfeed-search-filter-face       ((t (:foreground ,faded))))
   `(elfeed-search-last-update-face  ((t (:foreground ,salient))))
   `(elfeed-search-title-face        ((t (:foreground ,foreground :background ,background))))
   `(elfeed-search-tag-face          ((t (:foreground ,faded))))
   `(elfeed-search-unread-count-face ((t (:foreground ,foreground :bold t))))
   `(elfeed-search-unread-title-face ((t (:foreground ,foreground :bold t))))

   ;; Markdown
   `(markdown-blockquote-face         ((t (:inherit default))))
   `(markdown-bold-face               ((t (:foreground ,foreground :bold t))))
   `(markdown-code-face               ((t (:inherit default))))
   `(markdown-comment-face            ((t (:foreground ,faded))))
   `(markdown-footnote-marker-face    ((t (:inherit default))))
   `(markdown-footnote-text-face      ((t (:inherit default))))
   `(markdown-gfm-checkbox-face       ((t (:inherit default))))
   `(markdown-header-delimiter-face   ((t (:foreground ,faded))))
   `(markdown-header-face             ((t (:foreground ,foreground :bold t))))
   `(markdown-header-rule-face        ((t (:inherit default))))
   `(markdown-highlight-face          ((t (:inherit default))))
   `(markdown-hr-face                 ((t (:inherit default))))
   `(markdown-html-attr-name-face     ((t (:inherit default))))
   `(markdown-html-attr-value-face    ((t (:inherit default))))
   `(markdown-html-entity-face        ((t (:inherit default))))
   `(markdown-html-tag-delimiter-face ((t (:inherit default))))
   `(markdown-html-tag-name-face      ((t (:inherit default))))
   `(markdown-inline-code-face        ((t (:foreground ,popout))))
   `(markdown-italic-face             ((t (:foreground ,faded))))
   `(markdown-language-info-face      ((t (:inherit default))))
   `(markdown-language-keyword-face   ((t (:inherit default))))
   `(markdown-line-break-face         ((t (:inherit default))))
   `(markdown-link-face               ((t (:foreground ,salient))))
   `(markdown-link-title-face         ((t (:inherit default))))
   `(markdown-list-face               ((t (:foreground ,faded))))
   `(markdown-markup-face             ((t (:foreground ,faded))))
   `(markdown-math-face               ((t (:inherit default))))
   `(markdown-metadata-key-face       ((t (:foreground ,faded))))
   `(markdown-metadata-value-face     ((t (:foreground ,faded))))
   `(markdown-missing-link-face       ((t (:inherit default))))
   `(markdown-plain-url-face          ((t (:inherit default))))
   `(markdown-pre-face                ((t (:inherit default))))
   `(markdown-reference-face          ((t (:foreground ,salient))))
   `(markdown-strike-through-face     ((t (:foreground ,faded))))
   `(markdown-table-face              ((t (:inherit default))))
   `(markdown-url-face                ((t (:foreground ,salient))))
   `(markdown-header-face-1           ((t (:foreground ,foreground :bold t :height ,(nth 0 nano-theme-header-scales)))))
   `(markdown-header-face-2           ((t (:inherit markdown-header-face-1 :height ,(nth 1 nano-theme-header-scales)))))
   `(markdown-header-face-3           ((t (:inherit markdown-header-face-1 :height ,(nth 2 nano-theme-header-scales)))))
   `(markdown-header-face-4           ((t (:inherit markdown-header-face-1 :height ,(nth 3 nano-theme-header-scales)))))
   `(markdown-header-face-5           ((t (:inherit markdown-header-face-1 :height ,(nth 4 nano-theme-header-scales)))))
   `(markdown-header-face-6           ((t (:inherit markdown-header-face-1 :height ,(nth 5 nano-theme-header-scales)))))
   `(markdown-header-face-7           ((t (:inherit markdown-header-face-1 :height ,(nth 6 nano-theme-header-scales)))))

   ;; Notmuch
   `(notmuch-tag-face             ((t (:foreground ,faded))))
   `(notmuch-tag-unread           ((t (:foreground ,faded))))
   `(notmuch-search-date          ((t (:foreground ,faded))))
   `(notmuch-tag-deleted          ((t (:strike-through ,popout))))
   `(notmuch-tag-added            ((t (:underline ,popout))))
   `(notmuch-wash-cited-text      ((t (:foreground ,faded))))
   `(notmuch-message-summary-face ((t (:foreground ,strong :bold t :background ,subtle))))

   ;; Company
   `(company-tooltip                      ((t (:background ,subtle :foreground ,foreground))))
   `(company-tooltip-selection            ((t (:background ,popout))))
   `(company-tooltip-annotation           ((t (:foreground ,foreground))))
   `(company-tooltip-annotation-selection ((t (:foreground ,strong :bold t))))
   `(company-tooltip-common               ((t (:foreground ,strong))))
   `(company-tooltip-common-selection     ((t (:foreground ,strong :bold t))))
   `(company-scrollbar-bg                 ((t (:background ,faded))))
   `(company-scrollbar-fg                 ((t (:background ,foreground))))

   ;; Calendar
   `(calendar-today ((t (:foreground ,foreground :bold t))))

   ;; Mode Line
   `(mode-line          ((t ( :background ,background :overline ,strong
                              :box ,(if -modeline-pad `(:line-width ,-modeline-pad :color ,foreground))))))
   `(mode-line-inactive ((t ( :background ,background :foreground ,faded :overline ,subtle
                              :box ,(if -modeline-pad `(:line-width ,-modeline-pad :color ,faded))))))

   ;; tab-bar
   `(tab-bar                    ((t (:background ,subtle))))
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
