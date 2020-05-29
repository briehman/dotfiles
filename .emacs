(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(setq evil-want-C-i-jump nil)
(setq vc-follow-symlinks t)
(setq jiralib-url "https://jira.backstop.solutions")

;; Org
(add-hook 'org-mode-hook
	  (lambda ()
	    ;; Enable automatic line wrapping at fill column
	    (auto-fill-mode t)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(use-package iedit :ensure t)
(use-package magit
  :ensure t
  :config

  (global-set-key (kbd "C-x g") 'magit-status)
)

(use-package htmlize
  :ensure t
)

(use-package find-file-in-project
  :ensure t
  :config

  (global-set-key (kbd "C-x p") 'find-file-in-project)
)

(use-package doom-themes
  :ensure t
  :config

  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
  ;; theme may have their own settings.
  (load-theme 'doom-molokai t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!

  (require 'doom-modeline)
  (doom-modeline-mode 1)

)

(use-package all-the-icons
  :ensure t
)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (evil-escape-mode)
  (setq-default evil-escape-key-sequence "jk")

  (use-package evil-escape :ensure t)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "e" (lambda() (interactive)(find-file "~/.emacs"))
      "s" (lambda() (interactive)(load-file "~/.emacs"))
      "g" 'magit-status
    )
  )

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode 1)
  )
)

(use-package helm
  :ensure t
  :config

  (setq-default helm-M-x-fuzzy-match t)
  (global-set-key "\C-x\C-m" 'helm-M-x)
  (global-set-key "\C-c\C-m" 'helm-M-x)
  (global-set-key "\C-x\C-r" 'helm-recentf)

  (use-package evil
    :ensure t
    :config
    (define-key evil-ex-map "b " 'helm-mini)
    (define-key evil-ex-map "e" 'helm-find-files)
    (define-key evil-ex-map "r" 'helm-recentf)
    (define-key evil-ex-map "x" 'helm-M-x)
  )

)

(scroll-bar-mode -1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; hide startup
(setq inhibit-startup-screen t)

(setq org-default-notes-file "~/org/notes.org")
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" `org-agenda)

(setq org-agenda-files (directory-files-recursively "~/org/" "\.org$"))

(setq org-todo-keywords
      '((sequence "TODO(t!/!)" "IN_PROGRESS(i)" "WAITING(w)" "|" "DONE(d!/!)" "CANCELLED(c@/!)" "DELEGATED(g)" "FAILED(f@/!)")

	;; Sequence for POSSESSIONS
         ;; PURCHASE means to buy; it's functionally the wishlist
         ;; SELL means you want to get rid of it
         ;; DONATE means you want to get rid of it
         ;; UNWANTED is for no longer wanted
         ;; OWN is for stuff you actually own (may be overlap for reference and own)
         ;; GIFTED is given to someone as a gift
         ;; SOLD is sold to someone
         ;; DISCARDED is for thrown out
         ;; DONATED is for when it has been given away
         (sequence "PURCHASE(p@/!)" "SELL(k@/!)" "DONATE(@/!)" "|" "UNWANTED(a@/!)" "OWN(o@/!)" "GIFTED(g@/!)"  "SOLD(c@/!)" "DISCARDED(q@/!)" "DONATED(q@/!)")))

(setq org-todo-keyword-faces
      '(("CANCELLED" :foreground "grey" :weight bold)
	("DELEGATED" :foreground "light blue" :weight bold)
	("DONE" :foreground "light green" :weight bold)
	("FAILED" :foreground "red" :weight bold)
	("IN_PROGRESS" :foreground "dark violet" :weight bold)
	("PARTIALLY_DONE" :foreground "orange")
	("WAITING" :foreground "yellow" :weight bold)
	))

(setq org-agenda-custom-commands
      '(
        ("w" todo "WAITING")
        ("W" todo-tree "WAITING")
        ("ua" "Unscheduled TODOs" tags "+TODO=\"TODO\"&-SCHEDULED={.+}&-DEADLINE={.+}")
        ("uw" "Unscheduled Work TODOs" tags "+TODO=\"TODO\"&-SCHEDULED={.+}&-DEADLINE={.+}")
	))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-custom-commands
   (quote
    (("p" . "Personal agenda items")
     ("pa" "Personal agenda" agenda ""
      ((org-agenda-tag-filter-preset
	(quote
	 ("-work")))))
     ("w" . "Work agenda items")
     ("wa" "Work agenda" agenda ""
      ((org-agenda-tag-filter-preset
	(quote
	 ("+work")))))
     ("ww" "Waiting work items" todo "WAITING"
      ((org-agenda-tag-filter-preset
	(quote
	 ("+work")))))
     ("u" "Unscheduled TODOs" tags "+TODO=\"TODO\"&-SCHEDULED={.+}&-DEADLINE={.+}" nil)
     ("W" "Waiting items" todo "WAITING" nil))))
 '(org-agenda-todo-ignore-scheduled (quote future))
 '(org-capture-templates
   (quote
    (("p" "Personal item")
     ("pj" "Journal" entry
      (file+olp+datetree "~/org/journal.org")
      "* %?
Entered on %U
  %a")
     ("pn" "Notes" entry
      (file+olp+datetree "~/org/notes.org")
      "* %?
Entered on %U
  %i
  %a")
     ("pt" "Todo" entry
      (file+olp+datetree "~/org/personal.org" "Tasks")
      "* TODO %?
  %i")
     ("w" "Work item")
     ("wt" "TODO" entry
      (file+olp+datetree "~/org/backstop/agenda.org" "Tasks")
      "* TODO %?
  :LOGBOOK:
  - State \"TODO\" from %U
  :END:
  %i
")
     ("wh" "Header project task" entry
      (file+headline "~/org/backstop/projects/header.org" "Tasks")
      "* TODO %?
  %i")
     ("w1" "1:1 entry" entry
      (file+olp+datetree "~/org/refile.org" "1:1s")
      (file "~/org/templates/1on1.org")))))
 '(org-cycle-emulate-tab (quote white))
 '(org-default-priority 67)
 '(org-enforce-todo-dependencies t)
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-log-done t)
 '(org-log-into-drawer t)
 '(org-log-reschedule (quote time))
 '(org-lowest-priority 68)
 '(org-refile-allow-creating-parent-nodes (quote confirm))
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 3))))
 '(org-refile-use-outline-path (quote file))
 '(org-reverse-note-order (quote (("one-on-ones" . t))))
 '(safe-local-variable-values
   (quote
    ((org-todo-keyword-faces
      ("FAILED" . "red")
      ("PARTIALLY_DONE" . "orange")
      ("CANCELED" . "grey")
      ("DONE" . "light green")
      ("DELEGATED" . "light blue"))))))

(let ((default-directory  "~/.emacs.d/elisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'org-checklist)
 (add-to-list 'org-modules 'org-habit)

(org-babel-do-load-languages 'org-babel-load-languages
    '(
        (shell . t)
    )
)
