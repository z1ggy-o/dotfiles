;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;;
;; Personal configurations
;;
(when (eq system-type 'windows-nt) ; when OS is macOS
    (defun capture-from-win-clipboard()
    (interactive)
    (let* ((powershell "/mnt/c/Windows/System32\WindowsPowerShell/v1.0/powershell.exe")
        (file-name (format-time-string "screenshot_%Y%m%d_%H%M%s.png"))
        ;; (file-path-powershell (concat "c://User/\$env:USERNAME/" file-name))
        (file-path-wsl (concat "/home/ziggy/imgs/" file-name))
        )

        (shell-command (concat "powershell.exe -command \"(Get-Clipboard -Format Image).Save(\\\"" file-path-wsl "\\\")\""))
        (insert (concat "[[file:" file-path-wsl "]]"))
        (org-display-inline-images)
        ;;(shell-command (format "%s" mv-command))
        ;;(rename-file (concat "/mnt/c/Users/Public/" file-name) file-path-wsl)
        )))
(put 'dired-find-alternate-file 'disabled nil)

(use-package company
  :bind (:map company-active-map
         ("C-p" . company-selec-previous)
         ("C-n" . company-selec-next)
         ("<tab>" . company-complete-common-or-cycle)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :config
  (bind-key [remap completion-at-point] #'company-complete company-mode-map))
;;
;; Configurations for org-roam
;;
(setq org-roam-directory "~/org-roam-db")

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org-roam-db/"))

(use-package org-journal
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/org-roam-db/")
  (org-journal-date-format "%A, %d %B %Y"))

;; for C programming

(use-package lsp-mode
  :hook (prog-mode . lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-prefer-capf t)
  (lsp-auto-guess-root t)
  (lsp-keep-workspace-alive nil)
  :bind (:map lsp-mode-map
         ("C-c f" . lsp-format-region)
         ("C-c d" . lsp-describe-thing-at-point)
         ("C-c a" . lsp-execute-code-action)
         ("C-c r" . lsp-rename)))
