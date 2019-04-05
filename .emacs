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
(use-package magit :ensure t)
(use-package find-file-in-project
  :ensure t
  :config

  (global-set-key (kbd "C-x p") 'find-file-in-project)
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

(scroll-bar-mode -1)
(load-theme 'wombat)
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

(setq org-agenda-files
      (list
        "~/org/backstop/one-on-ones/ariel.org"
        "~/org/backstop/one-on-ones/ben.org"
        "~/org/backstop/one-on-ones/brian.org"
        "~/org/backstop/one-on-ones/doug.org"
        "~/org/backstop/one-on-ones/rich.org"
        "~/org/backstop/one-on-ones/rodrigo.org"
        "~/org/backstop/agenda.org"
        "~/org/gtd.org"
        ))

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d!/!)" "CANCELLED(c@/!)" "DELEGATED(g)" "FAILED(f@/!)")
	;; Sequence for MULTIMEDIA
         ;; CONSUME means to read (articles, books, quote, etc.), play (games), listen (music), or watch (a series or movie)
         ;; CONSUMING means currently consuming
         ;; CONSUMED means it has been completed
         ;; IGNORED means not read and no desire to read in the future
	(sequence "CONSUME(r@/!)" "CONSUMING(l@/!)" "|" "CONSUMED(x@/!)" "IGNORED(i@/!)")

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
	("ARTIALLY_DONE" :foreground "orange")
	("WAITING" :foreground "yellow")
	))

(setq org-agenda-custom-commands
      '(
        ("w" todo "WAITING")
        ("W" todo-tree "WAITING")
        ("u" "Unscheduled TODOs" tags "+TODO=\"TODO\"&-SCHEDULED={.+}")
	))

(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
        "* TODO %?\n  %i")
   ("b" "Backstop TODO" entry (file+datetree "~/org/backstop/agenda.org" "Tasks")
    "* TODO %?\n  %i\n")
   ("n" "Notes" entry (file+datetree "~/org/notes.org")
    "* %?\nEntered on %U\n  %i\n  %a")
   ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-todo-ignore-scheduled (quote future))
 '(org-hide-leading-stars t)
 '(org-log-done t)
 '(org-log-into-drawer t)
 '(org-refile-allow-creating-parent-nodes (quote confirm))
 '(org-refile-targets (quote ((org-agenda-files :level . 1))))
 '(org-refile-use-outline-path (quote file))
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
