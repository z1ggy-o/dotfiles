;; Tells emacs where can find the module files.
;; (add-to-list 'load-path "~/.emacs.d/modules/")

;; Package management
;; --------------------------
;; (require 'init-packages)

;; Use right command key as the meta key in macOS. The left side is not changed.
(when (eq system-type 'darwin) ; when OS is macOS
  (setq mac-right-command-modifier 'meta
	mac-command-modifier 'super))

(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (monokai-theme company org-journal deft org-roam))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; color theme
(use-package monokai-theme
  :ensure t)

(use-package org-roam
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/Zettelkasten")
      :bind (:map org-roam-mode-map
		  (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Zettelkasten/"))

(use-package org-journal
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/Zettelkasten/")
  (org-journal-date-format "%A, %d %B %Y"))

(use-package evil
  :ensure t
  :defer .1 ;; don't block emacs when starting
  :init  ;; tweak the plugin before load it
  (setq evil-want-integration nil) ;; required by evil-collection
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t) ;; like vim's 'splitright'
  (setq evil-split-window-below t) ;; like vim's 'splitbelow'
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config  ;; tweak evil after laoding it
  (evil-mode)
)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Let init.el can load this file
;; (provide 'init-packages)

;; Configuration from Master Emacs in 21 days

;; disable the toolbar
(tool-bar-mode -1)

;; disable the scrollbar
(scroll-bar-mode -1)

;; show line number
(global-linum-mode 1)

;; set cursor style
(setq cursor-type 'bar)

;; disable the help info when start emcas
(setq inhibit-splash-screen 1)

;; change font size to 16pt
(set-face-attribute 'default nil :height 160)

;; Use Company plugin in all the mode
(global-company-mode 1)

;; When highlighting some contents, new entry will substituts them like other editors do
(delete-selection-mode 1)

;; Open emacs as fullscreen
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; Highlight matching parenthesis
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;; Highlight the current line
(global-hl-line-mode 1)

;; Auto reload the file if the file is modified by other processes
(global-auto-revert-mode 1)

;; change the "yes or no" answer to "y or n"
(fset 'yet-or-no-p 'y-or-n-p)

(put 'dired-find-alternate-file 'disable nil)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;; Let Ctrl-w as backward kill a word
(global-set-key (kbd "C-w") 'backward-kill-word)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil monokai-theme company org-journal deft org-roam))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
