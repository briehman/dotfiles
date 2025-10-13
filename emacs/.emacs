(require 'package)

(add-to-list 'package-archives '("org" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(setq evil-want-C-i-jump nil)
(setq vc-follow-symlinks t)

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
  (setq magit-repository-directories '(("/Users/brian.riehman/dev/work/" . 1)))
)

(use-package htmlize
  :ensure t
)

(use-package find-file-in-project
  :ensure t
  :config

  (global-set-key (kbd "C-x p") 'find-file-in-project)
)

(set-frame-font "Monaco 15")
;; Default 160 -- increasing this keeps the org agenda to prefer bottom split
(setq split-width-threshold 100)

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
  :init
    (setq evil-want-keybinding nil)
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

  (use-package undo-tree
    :ensure t
    :config
    (evil-set-undo-system 'undo-tree)
  )

  ;; enable key bindings for other modes like the buffer list and such
  (use-package evil-collection
    :ensure t
    :config
    (evil-collection-init)
  )
)

(use-package undo-tree
:ensure t
:config
  (global-undo-tree-mode)
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

;; Indent text to org mode header on LHS
(setq org-adapt-indentation t)

(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" `org-agenda)

(setq org-agenda-files (directory-files-recursively "~/org/" "\.org$"))

(setq org-todo-keywords
      '((sequence "TODO(t!/!)" "IN_PROGRESS(i)" "WAITING(w)" "|" "DONE(d!/!)" "CANCELLED(c@/!)" "DELEGATED(g)" "FAILED(f@/!)")
         ))

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
	("u" "Unscheduled TODOs" tags "+TODO=\"TODO\"&-SCHEDULED={.+}&-DEADLINE={.+}")
	))

(setq org-capture-templates
      '(
	("t" "TODO" entry
	 (file+olp+datetree "~/org/agenda.org" "Tasks")
	 "* TODO %?\12  :LOGBOOK:\12  - State \"TODO\" from %U\12  :END:\12  %i\12")
	))

(global-set-key (kbd "C-c l") 'org-store-link)

(setq org-agenda-text-search-extra-files '(agenda-archives))
(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-cycle-emulate-tab 'white)
(setq org-default-priority 67)
(setq org-enforce-todo-dependencies t)
(setq org-export-backends '(ascii html icalendar latex md odt))
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
(setq org-log-done t)
(setq org-log-into-drawer t)
(setq org-log-reschedule nil)
(setq org-lowest-priority 68)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-reverse-note-order '(("one-on-ones" . t)))
(setq org-todo-keywords
   '((sequence "TODO(t!)" "IN_PROGRESS(i!)" "WAITING(w!)" "|"
	       "DONE(d!/!)" "CANCELLED(c@/!)" "DELEGATED(g)"
	       "FAILED(f@/!)")
     (sequence "PURCHASE(p!@/!)" "SELL(k@/!)" "DONATE(@/!)" "|"
	       "UNWANTED(a@/!)" "OWN(o@/!)" "GIFTED(g@/!)"
	       "SOLD(c@/!)" "DISCARDED(q@/!)" "DONATED(q@/!)")))

(setq safe-local-variable-values
   '((org-todo-keyword-faces ("FAILED" . "red")
			     ("PARTIALLY_DONE" . "orange")
			     ("CANCELED" . "grey")
			     ("DONE" . "light green")
			     ("DELEGATED" . "light blue"))))
(setq package-selected-packages
   '(helm all-the-icons-dired all-the-icons-ivy all-the-fonts
	  doom-modeline doom-themes use-package org-jira org-bullets
	  org-autolist magit iedit find-file-in-repository
	  find-file-in-project evil-surround evil-ledger evil-leader
	  evil-escape cl-generic))

(let ((default-directory  "~/.emacs.d/elisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(require 'org-checklist)

;; enable shell evaluation for babel
(org-babel-do-load-languages 'org-babel-load-languages
    '(
        (shell . t)
    )
)

(setq helm-minibuffer-history-key "M-p")

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
