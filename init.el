(setq inhibit-startup-message t)

(menu-bar-mode -1)   ;; Disable menu bar
(tool-bar-mode -1)   ;; Disable tool bar
(scroll-bar-mode -1) ;; Disable visible scrollbar
(tooltip-mode -1)    ;; Disable tooltips
(set-fringe-mode 10) ;; some spacing around

;; Flashes when you hit limits
(setq visible-bell t)

;; Display line numbers
(global-display-line-numbers-mode 1)
(column-number-mode)
;; Do not display line numbers in certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                treemacs-mode-hook
                shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Load customizable theme
(load-theme 'modus-vivendi t)

;; Make ESC quit prompts
;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

;; Don't pop up UI dialogs when prompting
(setq use-dialog-box nil)

;; Revert the buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers when files in folder added for example
(setq global-auto-revert-none-file-buffers t)

(set-face-attribute 'default nil :font "JetBrains Mono" :height 130)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 130)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 140)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;			 ("melpa-stable" . "https://stable.melpa.org/packges/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platfroms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Autocompletion
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivey-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibufer-history)))
;;  :config
;;  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

;; parences rainbow highliht
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Shows available keys at the bottom
(use-package which-key
  :init (which-key-mode)
  :diminish  which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Better Ivy / counsel print with rows of additional data
;; Generates a bunch of warning during install
;; But seems works. Maybe latest gihub version would do better.
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Should provide extended help but it does not somehow
(use-package helpful
  :custom
  (counsel-describe-function-funciton #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] .  helpful-key))

;; Suff for key bindings
(use-package general
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
    "tt" '(counsel-load-theme :which-key "choose-theme")))

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

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-project-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package direnv
 :config
 (direnv-mode))

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (julia . t)
   (python . t)
;;   (ein . t)
;;   (jupyter . t)
   ))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(defun my/org-babel-tangle-config ()
(when (string-equal (buffer-file-name)
                    (expand-file-name "~/.emacs.d/emacs.org"))
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui)

(use-package lsp-ivy)

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

(use-package eshell-git-prompt)
(use-package eshell
  :config
  (eshell-git-prompt-use-theme 'powerline))
