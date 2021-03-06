;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Guangyu Zhu"
      user-mail-address "guangyuzhu1129@gmail.com")

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
(setq doom-theme 'doom-gruvbox
      doom-font (font-spec :family "Sarasa Mono SC" :size 16))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; Set pdf viwer for different platform
(cond
 ((string-equal system-type "darwin")  ;; macOS
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "open" nil 0 nil "-a" "/Applications/PDF Expert.app" fpath)))
  )
 ((string-equal system-type "gnu/linux")
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "okular" nil 0 nil fpath))))
 )

;; Use command key as meta in macOS, that means we need to use Alt+C/V to copy/paste
(cond
 ((string-equal system-type "darwin")
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        ;;mac-option-modifier 'alt
        ;;mac-right-option-modifier 'alt
)))

;;
;; EVIL PART
;;
;;(after! evil-snipe (evil-snipe-mode -1))  ;; s for substitution comes back! ;; let's just use =c l=

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;;(setq org-directory "~/org/")

;; Some path variables
(setq org-directory "~/silverpath/org/"
      org-agenda-files (list "~/silverpath/org/" "\.org")
      org-archive-location (concat org-directory ".archive/%s::")

      roam_notes (concat org-directory "zettelkasten/")
      lit_notes (concat roam_notes "literature/")
      zot_bib(concat roam_notes "zotero.bib")
      paper_notes (concat roam_notes "paper_notes/")
      pdf_dir (concat org-directory "~/silverpath/papers/"))

;;
;; ORG PART
;;
;; for org-capture
(setq org-default-notes-file (concat org-directory "inbox.org"))
(after! org
  ;; Fix `org-cycle' bug
  (map! :map org-mode-map
        :n "<tab>" 'org-cycle)

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/silverpath/inbox.org" "Tasks")
          "* TODO %?\n %i\n %a")
          ("j" "Journal" entry (file+datetree "~/silverpath/org/journal.org")
          "* %U
** Today's Tasks
** Process
** Summary")
          ("n" "Note" entry (file+headline "~/silverpath/org/inbox.org" "Notes")
          "* %?\n%U\n%a\n")
          ("r" "Research Tasks" entry (file+headline "~/silverpaht/inbox.org" "Research Related")
          "* TODO %? \nLink: %a")
          ))
  (setq org-log-done t))

(add-hook 'org-mode-hook #'auto-fill-mode)  ;; enable auto-fill in all org files
(setq org-export-with-tags nil)  ;; when export org to other format, drop tags away

;;
;; ORG-ROAM PART
;;
(require 'time-stamp)  ;; for automatically add time stamp in org files
(add-hook 'write-file-functions 'time-stamp)

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :init
  (setq org-roam-directory roam_notes
        org-roam-db-location "~/.org-roam.db"  ;; move it out of sync directory
        org-roam-tag-sources '(prop last-directory)
        ;;org-roam-file-exclude-regexp "daily"
        )
  :config
  ;; org-roam templates
  (setq org-roam-capture-templates
        '(
          ("i" "inbox" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "inbox/%<%y%m%d>-${slug}"
           :head "#+title: ${title}\nTime-stamp: <>\n#+roam_alias:\n#+roam_tags:\n\n- tags ::\n- source ::"
           :unnarrowed t)
          ("l" "literature: book, blog, web..." plain (function org-roam-capture--get-point)
           "%?"
           :file-name "literature/%<%y%m%d>-${slug}"
           :head "#+title: ${title}\nTime-stamp: <>\n#+roam_alias:\n#+roam_tags:\n\n- tags ::\n- source :: "
           :unnarrowed t)
          ("c" "concept" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "concept/%<%y%m%d>-${slug}"
           :head "#+title: ${title}\nTime-stamp: <>\n#+roam_alias:\n#+roam_tags:\n\n- tags :: "
           :unnarrowed t)
          ("o" "outlines" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "outlines/%<%y%m%d>-${slug}"
           :head "#+title: ${title}\nTime-stamp: <>\n#+roam_alias:\n#+roam_tags:\n\n- tags :: "
           :unnarrowed t)
          )
        )
  ;; capture website from browser
  ;; =+roam_key= is used for "ref backlink"
  (setq org-roam-capture-ref-templates
      '(("r" "ref" plain (function org-roam-capture--get-point)
         "%?"
         :file-name "literature/%<%y%m%d>-${slug}"
         :head "#+title: ${title}
Time-stamp: <>
#+roam_key: ${ref}
#+roam_tags: website

- source :: ${ref}"
         :unnarrowed t)
        ("a" "Annotation" plain (function org-roam-capture--get-point)
         "%U ${body}\n"
         :file-name "literature/%<%y%m%d>-${slug}"
         :head "#+title: ${title}
Time-stamp: <>
#+roam_key: ${ref}
#+roam_tags: website

- source :: ${ref}"
         :immediate-finish t
         :unnarrowed t)))
  ;;
  ;; org-roam-daily
  ;;
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(
          ("j" "journal" entry
           #'org-roam-capture--get-point
           "* [%<%H:%M:%S>] %?"
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+title: %<%Y-%m-%d>\n"
           :olp ("Journal"))

          ;; ("k" "knowledge" entry
          ;;  #'org-roam-capture--get-point
          ;;  "* %? [%<%H:%M:%S>]"
          ;;  :file-name "daily/%<%Y-%m-%d>"
          ;;  :head "#+title: %<%Y-%m-%d>\n"
          ;;  :olp ("Knowledge notes"))
          ))

  (map! :leader
        (:prefix "n"
         (:prefix "r"
          :desc "Capture for today" "j" #'org-roam-dailies-capture-today))))

;;
;; ORG-ROAM-SERVER
;;
(use-package! org-roam-server
  :after org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)
)
;; alway enable org-roam-server with org-roam
(after! org-roam
  (org-roam-server-mode))

;;
;; ORG-ROAM-BIBTEX PART
;;
(use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  ;; This may let us get the true contents of these keywords from bibtex
  ;;(setq org-roam-bibtex-preformat-keywords
  (setq orb-preformat-keywords
        '("=key=" "title" "url" "file" "author-or-editor" "keywords"))

  (setq orb-templates
        '(
          ("r" "ref + note" plain (function org-roam-capture--get-point)
           ""
           :file-name "paper_notes/${=key=}"
           :head "#+TITLE: ${=key=}: ${title}
#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

* ${title}
:PROPERTIES:
:Custom_ID: ${=key=}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:END:

** Why
why I read this paper?
- background and related work?
- good writing/learning tempalte
- good figure
- good evaluation

** Short Summary
As short as possible, may contain links

** What Is the Problem

** Why the Problem Is Interesting

** The Idea

* Reading Notes
:PROPERTIES:
:NOTER_DOCUMENT: ${file}
:NOTER_PAGE:
:END:"

           :unnarrowed t)
          ("b" "book" plain (function org-roam-capture--get-point)
           ""
           :file-name "outlines/${=key=}"
           :head "#+TITLE: ${=key=}: ${title}
#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

* ${title}
:PROPERTIES:
:Custom_ID: ${=key=}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:END:

* Reading Notes
:PROPERTIES:
:NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")
:NOTER_PAGE:
:END:"

           :unnarrowed t)
          ))
  (define-key org-roam-bibtex-mode-map (kbd "C-c m a") #'orb-note-actions)

  )

;;
;; ORG-REF PART
;;
(use-package! org-ref
  :config
  ;; set the bibtex file for org-ref
  (setq reftex-default-bibliography (list (concat roam_notes "zotero.bib")
                                  (concat roam_notes "calibre_books.bib")))
  ;; set notes, pdf directory
  (setq org-ref-bibligraphy-notes (concat lit_notes "bibnotes.org")
  org-ref-notes-directory paper_notes
  org-ref-default-bibliography zot_bib
  org-ref-pdf-directory pdf_dir)
  (setq
  ;;org-ref-completion-library 'org-ref-ivy-cite
  ;; use helm-bibtex to find pdf
  org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
  ;; use org-roam's note setting instead of org-ref's
  org-ref-notes-function 'orb-edit-notes
  ;;org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
  )
)

;;
;; helm-bibtex part
;;
(after! org-ref
  (use-package! helm-bibtex
  :commands helm-bibtex
  :config
  (setq
   bibtex-completion-bibliography (list (concat roam_notes "zotero.bib")
                                      (concat roam_notes "calibre_books.bib"))
   bibtex-completion-notes-path paper_notes
   bibtex-completion-library-path pdf_dir
   bibtex-completion-pdf-field "file"  ;; filed in bibtex to help find related pdf. optional
   bibtex-completion-notes-template-multiple-files
   (concat
           "#+title: ${title}\n"
           "#+roam_key: cite:${=key=}\n"
           "* todo notes\n"
           ":properties:\n"
           ":custom_id: ${=key=}\n"
           ":noter_document: %(orb-process-file-field \"${=key=}\")\n"
           ":author: ${author-abbrev}\n"
           ":journal: ${journaltitle}\n"
           ":date: ${date}\n"
           ":year: ${year}\n"
           ":doi: ${doi}\n"
           ":url: ${url}\n"
           ":end:\n\n"
           )
   )
  (global-set-key (kbd "C-c h") 'helm-bibtex)
))

;;
;; DEFT PART
;;
(use-package! deft
  :commands deft
  :config
  (setq deft-default-extension "org"
        deft-directory roam_notes
        ;; recursively research notes under the directory
        deft-recursive t
        ;; decouples file name and note title:
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        ;; disable auto-save
        deft-auto-save-internal -1.0
        ;; converts the filter string into a readable file-name using kebab-case:
        deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase))
        )
  (global-set-key (kbd "C-c m d") 'deft)
  (setq deft-auto-save-interval 20)
  )

;;
;; ORG-NOTER PART
;;
(use-package! org-noter
  :config
  (setq
   ;; The WM can handle splits
   ;; org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path (list roam_notes)
   org-noter-separate-notes-from-heading t ;; create a empty line between heading and note paragraphy
   )
  )

;; pdf-tools part
;;
;;
(use-package! pdf-tools
  :bind (:map pdf-view-mode-map
         (("h" . pdf-annot-add-highlight-markup-annotation)
         ("u" . pdf-annot-add-underline-markup-annotation)
         ("t" . pdf-annot-add-text-annotation)
         ("d" . pdf-annot-delete)))
  ;;((kbd "h") #'pdf-annot-add-highlight-markup-annotation)
  ;;((kbd "u") #'pdf-annot-add-underline-markup-annotation)
  ;;((kbd "t") #'pdf-annot-add-text-annotation)
  ;;((kbd "d") #'pdf-annot-delete)
  ;;)
  ;;:config
  ;;(setq pdf-view-use-scaling t)  ;; doom-emacs has already done this for us
)
  ;;(define-key pdf-view-mode-map (kbd "h") #'pdf-annot-add-highlight-markup-annotation)
  ;;(define-key pdf-view-mode-map (kbd "u") #'pdf-annot-add-underline-markup-annotation)
  ;;(define-key pdf-view-mode-map (kbd "t") #'pdf-annot-add-text-annotation)
  ;;(define-key pdf-view-mode-map (kbd "d") #'pdf-annot-delete)

;;
;; for ccls
;;
(setq ccls-executable "/usr/local/bin/ccls")

;;
;; ox-hugo part
;; Populates only the EXPORT_FILE_NAME property in the inserted headline.
;;
(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("h"                ;`org-capture' binding + h
                 "Hugo post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of all-posts.org!
                 (file+olp "zettelkasten/blogs/articles.org" "New Post")
                 (function org-hugo-new-subtree-post-capture-template))))

;; ORG-DOWNLOAD

;; from https://zzamboni.org/post/my-doom-emacs-configuration-with-commentary/ and
;; https://emacs-china.org/t/emacs/15765/9
;;
;; This function help us to give a name to the capture before we call org-download-clipboard
;; to save the file for us.
;;
;; ask for the filename before pasting an image
;; filename should end with ".png/.jpg/.svg"
(defun zz/org-download-paste-clipboard (&optional use-default-filename)
  (interactive "P")
  (require 'org-download)
  (let ((file
         (if (not use-default-filename)
             (read-string (format "Filename [%s]: "
                                  org-download-screenshot-basename)
                          nil nil org-download-screenshot-basename)
           nil)))
    (org-download-clipboard file)))

(use-package! org-download
  :after org
  :config
  (setq-default org-download-method 'directory
                org-download-image-dir "~/.images/"
                org-download-heading-lvl nil
                org-download-timestamp "%Y%m%d_"
                org-image-actual-width 300)
  (map! :map org-mode-map
        :localleader "ap" #'zz/org-download-paste-clipboard)
)
