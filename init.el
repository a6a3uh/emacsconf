(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-vesrion 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package no-littering)

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(cl-case system-type
  ('gnu/linux (setq yadisk-path "~/yadisk"
                    org-path "~/org"))
  ('windows-nt (setq yadisk-path "~/YandexDisk"
                     org-path "~/YandexDisk/org"))
  )

(setq my/bib-files (list (concat yadisk-path "/phd/phd.bib")))

(setq inhibit-startup-message t)

(menu-bar-mode -1)   ;; Disable menu bar
(tool-bar-mode -1)   ;; Disable tool bar
(scroll-bar-mode -1) ;; Disable visible scrollbar
(tooltip-mode -1)    ;; Disable tooltips
(set-fringe-mode 10) ;; some spacing around

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Flashes when you hit limits
(setq visible-bell t)

(column-number-mode)

;; Open recent files
(recentf-mode 1)

;; Save what you entered into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))

(load custom-file 'noerror 'nomessage)

;; Revert the buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers when files in folder added for example
(setq global-auto-revert-none-file-buffers t)

;; Load customizable theme
(load-theme 'modus-vivendi t)

(set-face-attribute 'default nil :font "JetBrains Mono" :height 130)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 130)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 140)

(use-package vertico
  :init
  (vertico-mode +1))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-comletion)))))

;; to show additional columns of info
(use-package marginalia
  :config (marginalia-mode))

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; parences rainbow highliht
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Shows available keys at the bottom
(use-package which-key
  :defer 0
  :diminish  which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

;; Should provide extended help but it does not somehow
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] .  helpful-key))

;; Suff for key bindings
(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (general-create-definer rune/leader-keys
    :keymaps '(normal insrt visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; Here bind some keys
  (general-define-key
   (kbd "<escape>") 'keyboard-escape-quit)

  (rune/leader-keys
    "t" '(:ignore t :which-key "toggles")
    "tt" '(consult-theme :which-key "choose-theme")
    "f" '(:ignore t :which-key "files")
    "fr" '(consult-recent-file :which-key "recent files")
    "fd" '(dired :which-key "dired")
    "w" '(ace-window :which-key "windows")))

;; Previews stuff and plays nicely with vertico or similar
(use-package consult
  :general
  ("M-y" 'consult-yank-from-kill-ring
   "C-x b" 'consult-buffer))

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package dired-single)

(use-package dired
  :after evil-collection
  :straight nil
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package dired-hide-dotfiles
  :after evil-collection
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package ace-window)

;; Good mode with bad name
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump  nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Make ESC quit prompts
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-project-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

(when (equal system-type 'gnu/linux)
  (use-package direnv
     :config
     (direnv-mode)))

(use-package ranger
  :straight t
    :after dired)

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; This is to login to GitHub and work with issues etc
;;(use-package forge)

(defun my/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1)
  ;; (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . my/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t
	org-src-fontify-natively t))

;; Don't request confirm when evaluating certaing languages
(defun my/org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "jupyter-python") (string= lang "jupyter-julia"))))
(setq org-confirm-babel-evaluate 'my/org-confirm-babel-evaluate)

(defun my/org-babel-tangle-config ()
(when (string-equal (buffer-file-name)
                    (expand-file-name "~/.emacs.d/emacs.org"))
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config)))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;; :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package engrave-faces)
(setq org-latex-src-block-backend 'engraved)

(use-package julia-mode)
;; (require 'julia-mode)

(use-package lsp-julia)

(use-package  julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode
(add-hook 'julia-mode-hook #'lsp-mode)
;; (add-hook 'julia-mode-hook (setq-local lsp-enable-folding t
;;                                        lsp-folding-range-limit 100))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; 'C-l', 's-l'
  (setq lsp-prefer-flymake nil)
  :config
  (lsp-enable-which-key-integration t)
  :hook
  ((python-mode . lsp)))

(use-package lsp-ui)

(use-package consult-lsp)

(use-package jupyter)

(use-package ein)

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (julia . t)
     (python . t)
     (ein . t)
     (jupyter . t)
     ))
  (push '("conf-unix" . conf-unix) org-src-lang-modes) )

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package lsp-treemacs
  :after lsp)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package flycheck)

(when (equal system-type 'gnu/linux)
  (use-package vterm))

(use-package eshell-git-prompt)
(use-package eshell
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package org-fragtog)
(add-hook 'org-mode-hook 'org-fragtog-mode)

(use-package pdf-tools
  :config
  (pdf-tools-install)
  )
(add-hook 'pdf-view-mode-hook (blink-cursor-mode -1))

(use-package org-roam
  :straight t
  :custom
  (org-roam-directory (concat org-path "/roam"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point))
  :config
  (org-roam-setup))

(use-package citar
  :after oc
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (citar-bibliography my/bib-files)
  (citar-library-paths (list (concat yadisk-path "/phd/papers")))
  (citar-notes-paths (list (concat org-path "/roam/references")))
  (citar-file-extensions '("pdf" "org" "md"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-cite-export-processors '((latex biblatex) (t csl)))
  (org-support-shift-select t)
  (org-cite-global-bibliography my/bib-files)
  )

(use-package org-ref)

(use-package org-roam-bibtex)

(use-package djvu)

(setq
 org-startup-with-latex-preview t
)

(when (equal system-type 'gnu/linux)
  (use-package mu4e
    :straight nil
    :load-path "/usr/share/emacs/site-lisp/mu4e/"
    :defer 20 ; Wait until 20 seconds after startup
    :config
    (mu4e t)

    ;; (setq +mu4e-gmail-accounts '(("ali.tlisov@gmail.com" . "/Gmail")))
    (setq mu4e-headers-buffer-name "*mu4e-headers*")
    ;; This is set to 't' to avoid mail syncing issues when using mbsync
    (setq mu4e-change-filenames-when-moving t)

    ;; Refresh mail using isync every 10 minutes
    (setq mu4e-update-interval (* 10 60))
    (setq mu4e-get-mail-command "mbsync gmail")
    (setq mu4e-maildir "~/Mail")

    (setq mu4e-drafts-folder "/Drafts")
    (setq mu4e-sent-folder   "/Sent")
    (setq mu4e-refile-folder "/Archive")
    (setq mu4e-trash-folder  "/Trash")

    ;; (setq mu4e-maildir-shortcuts
    ;;       '(("/Inbox" . ?i)
    ;;         ("/Sent" . ?s)
    ;;         ("/Trash" . ?t)
    ;;         ("/Drafts" . ?d)
    ;;         ("/Archive" . ?a)))
    ;;
    ))
