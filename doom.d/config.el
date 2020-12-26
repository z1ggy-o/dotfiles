;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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
(setq doom-theme 'doom-one
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

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;;(setq org-directory "~/org/")

;; Some path variables
(setq org-directory "~/silverpath/org/"
      org-agenda-files (list "~/silverpath/org/" "\.org$")
      org-archive-location (concat org-directory ".archive/%s::")
      roam_notes "~/silverpath/org-roam-db/"
      lit_notes "~/silverpath/org-roam-db/literature/"
      zot_bib "~/silverpath/org-roam-db/zotero.bib"
      paper_notes "~/silverpath/org-roam-db/paper_notes/"
      pdf_dir "~/silverpath/papers/")

;;
;; ORG PART
;;
;; for org-capture
(setq org-default-notes-file (concat org-directory "inbox.org"))
(after! org
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

;;
;; ORG-ROAM PART
;;
(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :init
  (setq org-roam-directory roam_notes
        org-roam-db-location "~/.org-roam.db"  ;; move it out of sync directory
        org-roam-tag-sources '(prop last-directory))
  :config
  ;; org-roam templates
  (setq org-roam-capture-templates
      '(
        ("i" "inbox" plain (function org-roam-capture--get-point)
         "%?"
         :file-name "inbox/%<%y%m%d>-${slug}"
         :head "#+title: ${title}\n#+roam_alias:\n#+roam_tags:\n\n- tags ::\n- source ::"
         :unnarrowed t)
        ("l" "literature: book, blog, web..." plain (function org-roam-capture--get-point)
         "%?"
         :file-name "literature/${slug}"
         :head "#+title: ${title}\n#+roam_alias:\n#+roam_tags:\n\n- tags ::"
         :unnarrowed t)
        ("c" "concept" plain (function org-roam-capture--get-point)
         "%?"
         :file-name "concept/${slug}"
         :head "#+title: ${title}\n#+roam_alias:\n#+roam_tags:\n\n- tags :: "
         :unnarrowed t)
        ("o" "outlines" plain (function org-roam-capture--get-point)
         "%?"
         :file-name "outlines/${slug}"
         :head "#+title: ${title}\n#+roam_alias:\n#+roam_tags:\n\n- tags :: "
         :unnarrowed t)
        )
      )
  (setq org-roam-capture-ref-templates
      '(("r" "ref" plain (function org-roam-capture--get-point)
         "%?"
         :file-name "literature/${slug}"
         :head "#+title: ${title}
#+roam_key: ${ref}
#+roam_tags: website

- source :: ${ref}"
         :unnarrowed t)
        ("a" "Annotation" plain (function org-roam-capture--get-point)
         "%U ${body}\n"
         :file-name "literature/${slug}"
         :head "#+title: ${title}
#+roam_key: ${ref}
#+roam_tags: website

- source :: ${ref}"
         :immediate-finish t
         :unnarrowed t)))
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
;; RG-ROAM-BIBTEX PART
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

** Short Summary

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
                 (file+olp "blogs/inbox.org" "Blog Ideas")
                 (function org-hugo-new-subtree-post-capture-template))))
