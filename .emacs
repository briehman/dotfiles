(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

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
      '((sequence "TODO(t)" "|" "DONE(d!/!)" "CANCELLED(c@/!)" "DELEGATED(g)")))

(setq org-todo-keyword-faces
      '(("CANCELLED" :foreground "grey" :weight bold)
	("FAILED" :foreground "red" :weight bold)
	("PARTIALLY_DONE" :foreground "orange")
	("DONE" :foreground "light green" :weight bold)))

(setq org-capture-templates
 '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
        "* TODO %?\n  %i\n  %a")
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
 '(safe-local-variable-values
   (quote
    ((org-todo-keyword-faces
      ("FAILED" . "red")
      ("PARTIALLY_DONE" . "orange")
      ("CANCELED" . "grey")
      ("DONE" . "light green")
      ("DELEGATED" . "light blue"))))))
