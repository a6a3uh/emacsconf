(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(setq package-enable-at-startup nil)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

(use-package no-littering)

(use-package exec-path-from-shell
  :init
  (when (not (eq system-type 'windows-nt))
    (exec-path-from-shell-initialize))
  )



;; (cl-case system-type
  ;; ('gnu/linux (setq yadisk-path "~/Yandex.Disk"
                    ;; org-path "~/org"
                    ;; root-path "/snap/bin/root"))
  ;; ('windows-nt (setq yadisk-path "Z:"
                     ;; org-path (concat yadisk-path "/org")
                     ;; root-path "C:\\root_v6.28.00\\bin\\root.exe")))
;; 
;; (setq my/bib-files (list (concat yadisk-path "/papers/phd.bib")))
;; (setq my/pdf-files (list (concat yadisk-path "/papers/papers")))

(use-package rg)

(setq-default buffer-file-coding-system 'utf-8)
(setq-default coding-system-for-read 'utf-8)
(prefer-coding-system 'utf-8)

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
(add-to-list 'savehist-additional-variables 'org-ai-openai-api-token)
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
;; (load-theme 'modus-operandi t)

(set-face-attribute 'default nil :font "JetBrains Mono" :height 150)
(set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 150)

(use-package lambda-themes
  :elpaca (:type git :host github :repo "lambda-emacs/lambda-themes") 
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t) 
  :config
  ;; load preferred theme 
  (load-theme 'lambda-light)
  ;; (load-theme 'modus-vivendi)
  )

(use-package lambda-line
  :elpaca (:type git :host github :repo "lambda-emacs/lambda-line") 
  :custom
  (lambda-line-icon-time t) ;; requires ClockFace font (see below)
  (lambda-line-clockface-update-fontset "ClockFaceRect") ;; set clock icon
  (lambda-line-position 'top) ;; Set position of status-line 
  (lambda-line-abbrev t) ;; abbreviate major modes
  (lambda-line-hspace "  ")  ;; add some cushion
  (lambda-line-prefix t) ;; use a prefix symbol
  (lambda-line-prefix-padding nil) ;; no extra space for prefix 
  (lambda-line-status-invert nil)  ;; no invert colors
  (lambda-line-gui-ro-symbol  " ⨂") ;; symbols
  (lambda-line-gui-mod-symbol " ⬤") 
  (lambda-line-gui-rw-symbol  " ◯") 
  (lambda-line-space-top +.50)  ;; padding on top and bottom of line
  (lambda-line-space-bottom -.50)
  (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
  :config
  ;; activate lambda-line 
  (lambda-line-mode) 
  ;; set divider line in footer
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_"))))

(use-package vertico
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  :config
  (vertico-mode))

(define-key function-key-map [(control tab)] [?\M-\t])
(when (fboundp 'w32-register-hot-key) (w32-register-hot-key [M-tab]))

(use-package orderless
  :custom
  (completion-styles '(orderless))      ; Use orderless
)

;; to show additional columns of info
(use-package marginalia
  :config (marginalia-mode))

;(use-package all-the-icons-completion
;  :after (marginalia all-the-icons)
;  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
;  :init
;  (all-the-icons-completion-mode))

(use-package embark
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
    "g" '(magit :which-key "magit")
    "t" '(:ignore t :which-key "toggles")
    "tt" '(consult-theme :which-key "choose-theme")
    "tz" '(writeroom-mode :which-key "zen mode")
    "f" '(:ignore t :which-key "files")
    "ff" '(consult-recent-file :which-key "recent files")
    "fd" '(dired :which-key "dired")
    "fr" '(dirvish :which-key "dirvish")
    "tn" '(display-line-numbers-mode :which-key "line numbers")
    "p" '(consult-projectile :which-key "select projects")
    "b" '(consult-buffer :which-key "select buffer")
    "s" '(:ignore t :which-key "search")
    ;; "sr" '(my/org-roam-rg-search :which-key "search roam files")
   ; "w" '(ace-window :which-key "windows")
   ))

;; Previews stuff and plays nicely with vertico or similar
(use-package consult
  )

;(defun my/org-roam-rg-search ()
;  "Search org-roam directory using consult-ripgrep. With live-preview."
;  (interactive)
;  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
;    (consult-ripgrep org-roam-directory)))
;; (global-set-key (kbd "C-c rr") 'my/org-roam-rg-search)

(use-package embark-consult)

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

;(use-package doom-modeline
;  :straight t
;  :init (doom-modeline-mode 1)
;  :custom ((doom-modeline-height 15)))

;(use-package all-the-icons
;  :if (display-graphic-p))

(use-package dired-single)

;(use-package dired
 ; :after evil-collection
;  :straight nil
  ;:config
  ;(evil-collection-define-key 'normal 'dired-mode-map
  ;  "h" 'dired-single-up-directory
  ;  "l" 'dired-single-buffer)
;  )

;(use-package all-the-icons-dired)
;(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package dired-hide-dotfiles
  :after evil-collection
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode)
 )

;(use-package mixed-pitch
;  :hook
;  ;; If you want it in all text modes:
;  (text-mode . mixed-pitch-mode))

;(use-package ace-window
;  :straight t)
;(setq aw-dispatch-always t)

(use-package writeroom-mode)

(use-package undo-tree
  :init
  (global-undo-tree-mode))

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
  (evil-set-initial-state 'dashboard-mode 'normal)

  (evil-set-undo-system 'undo-tree)
  ;; Turn off unused stuff to release C-. for embark
  (define-key evil-normal-state-map (kbd "C-.") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;(use-package evil-textobj-tree-sitter
;  :straight t)

;(use-package projectile
;  :diminish projectile-mode
;  :config (projectile-mode +1)
;  :bind-keymap
;  ("C-c p" . projectile-command-map)
;  :init
;  (when (file-directory-p "~/code")
;    (setq projectile-project-search-project-path '("~/code")))
;  (setq projectile-switch-project-action #'projectile-dired))

;(use-package consult-projectile
;  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

;(when (equal system-type 'gnu/linux)
;  (use-package direnv
;     :config
;     (direnv-mode)))

(use-package dirvish
  :after evil-collection
  :init (dirvish-override-dired-mode)
  :custom
  (dirvish-preview-disabled-exts '("org"))
  :config
  (evil-collection-define-key 'normal 'dirvish-mode-map
    "q" 'dirvish-quit)
)

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)
            ;; (dired-sort-toggle-or-edit)
            ))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(defun my/org-mode-setup ()
  (org-indent-mode)
  (valign-mode)
  ;; (variable-pitch-mode 1)
  ;; (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (setq org-image-actual-width nil)
  )

(use-package org
  :defer t
  :elpaca nil
  :hook (org-mode . my/org-mode-setup)
  ;; :custom
  ;; (org-latex-compiler "xelatex")
  :config
  (require 'org-inlinetask)
  (setq org-ellipsis " ▾"
	;; org-hide-emphasis-markers t
	org-src-fontify-natively t))

;; Don't request confirm when evaluating certaing languages
(defun my/org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "jupyter-python")
           (string= lang "python")
           (string= lang "dot")
           (string= lang "julia")
           (string= lang "jupyter-julia")
           (string= lang "latex")
           (string= lang "C++")
           (string= lang "cern-root")
           (string= lang "emacs-lisp"))))
(setq org-confirm-babel-evaluate 'my/org-confirm-babel-evaluate)

(defun my/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-config)))

;(use-package org-roam-ui
;  :straight
;  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
;  :after org-roam
;  :config
;  (setq org-roam-ui-sync-theme t
;        org-roam-ui-follow t
;        org-roam-ui-update-on-save t
;        org-roam-ui-open-on-start t))

(use-package auctex
  :elpaca  (auctex :pre-build (("./autogen.sh")
                               ("./configure" "--without-texmf-dir" "--with-lispdir=.")
                               ("make")))
  :mode (("\\.tex\\'" . LaTeX-mode)
         ("\\.tex\\.erb\\'" . LaTeX-mode)
         ("\\.etx\\'" . LaTeX-mode))
  :hook (text-mode-hook . (lambda ()
			    (load "auctex.el")
			    (setq TeX-command-extra-options "-shell-escape")))
  :config
  (setq-default TeX-global-PDF-mode 1)
  (setq-default  preview-scale-function 1.5)
  (setq TeX-auto-save t
        TeX-parse-self t
        default-truncate-lines t
        TeX-save-query nil
        TeX-source-correlate-method 'synctex)
  (add-hook 'TeX-mode-hook
            (lambda ()
              (setq TeX-command-extra-options "-shell-escape"))))

(use-package cdlatex)
(add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook #'turn-on-cdlatex)   ; with Emacs latex mode

(use-package engrave-faces)
(setq org-latex-src-block-backend 'engraved)

(require 'org)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8))

(defun jupyter-julia-trim-latex (orig-fun data)
  (apply orig-fun (list (string-trim data "[ $]+" "[ $]+"))))

(advice-add 'jupyter-org--parse-latex-element :around #'jupyter-julia-trim-latex)

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("pj" . "src jupyter-python"))
(add-to-list 'org-structure-template-alist '("jl" . "src julia"))
(add-to-list 'org-structure-template-alist '("jj" . "src jupyter-julia"))
(add-to-list 'org-structure-template-alist '("cpp" . "src C++"))
(add-to-list 'org-structure-template-alist '("root" . "src cern-root"))
(add-to-list 'org-structure-template-alist '("dot" . "src dot"))

(use-package org-remark)

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(use-package valign)

(use-package pyvenv)

(custom-set-variables '(python-shell-interpreter "ipython"))

;(use-package cern-root-mode
;  :after org
;  :bind (:map c++-mode-map
;             (("C-c C-c" . cern-root-eval-defun)
;              ("C-c C-b" . cern-root-eval-buffer)
;              ("C-c C-l" . cern-root-eval-file)
;              ("C-c C-r" . cern-root-eval-region)))
;  :straight (cern-root-mode :type git :host github :repo "jaypmorgan/cern-root-mode")
;  :config
;  (setq cern-root-filepath root-path))
;  ;(require 'cern-root-mode)

(use-package eglot
    :ensure t
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-to-list 'eglot-server-programs '(python-mode . ("jedi-language-server")))
    )

(use-package eglot-jl
    :ensure t)

(use-package julia-mode
    :ensure t
    :mode "\\.jl\\'"
    :interpreter "julia"
    :config
    (eglot-jl-init)
    ;; Specify the hook that connects =eglot=
    :hook (julia-mode . eglot-ensure))

(use-package  julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode
(add-hook 'julia-mode-hook 'company-mode)
(add-hook 'julia-mode-hook 'company-quickhelp-mode)
;; (add-hook 'julia-mode-hook 'ts-fold-indicators-mode)

;(use-package lsp-julia
;  :config
;  (setq lsp-julia-default-environment "~/.julia/environments/v1.9"))

(use-package cmake-mode)

;(use-package yasnippet)

;  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)

;(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;(add-hook 'c-mode-hook 'eglot-ensure)
;(add-hook 'c++-mode-hook 'eglot-ensure)
;(add-to-list 'eglot-server-programs '(python-mode . ("jedi-language-server")))

;(add-hook 'python-mode-hook 'eglot-ensure)
;(add-hook 'julia-mode-hook 'eglot-ensure)
;(add-hook 'julia-mode-hook 'eglot-jl-init)

(use-package consult-eglot
  :after
  eglot
  consult
  )


;(use-package lsp-mode)
;(use-package dap-mode)

;(use-package consult-lsp)

;(which-key-mode)
;(add-hook 'c-mode-hook 'lsp)
;(add-hook 'c++-mode-hook 'lsp)

;; (add-hook 'julia-mode-hook 'lsp)

;(with-eval-after-load 'lsp-mode
;  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;  (require 'dap-cpptools)
;  ;(yas-global-mode)
;  )

(use-package jupyter
  :after org
  )

(with-eval-after-load 'jupyter
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (julia . t)
     (python . t)
     ;;(ein . t)
     (C . t)
     (dot . t)
     (jupyter . t)
     ))
  (org-babel-jupyter-override-src-block "julia")
  (push '("conf-unix" . conf-unix) org-src-lang-modes) )


(use-package company
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-quickhelp)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;(when (equal system-type 'gnu/linux)
;  (use-package vterm))

;(use-package eshell-git-prompt)

;(use-package eshell
  ;:config
  ;(eshell-git-prompt-use-theme 'powerline)
;  )

;; (use-package pdf-tools
 ;; :config
 ;; (pdf-tools-install)
 ;; )

(add-hook 'pdf-view-mode-hook (blink-cursor-mode -1))

;;(use-package org-roam
;;  :custom
;;  (org-roam-directory (concat org-path "/roam"))
;;  (org-roam-completion-everywhere t)
;;  :bind (("C-c n l" . org-roam-buffer-toggle)
;;         ("C-c n f" . org-roam-node-find)
;;         ("C-c n i" . org-roam-node-insert)
;;         :map org-mode-map
;;         ("C-M-i" . completion-at-point))
;;  :config
;;  (org-roam-setup))

(use-package citar
  :after oc
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  ;; (citar-bibliography my/bib-files)
  ;; (citar-library-paths my/pdf-files)
  ;;(citar-notes-paths (list (concat org-path "/roam/references")))
  (citar-file-extensions '("pdf" "org" "md"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (org-cite-export-processors '((latex biblatex) (t csl)))
  (org-support-shift-select t)
  (org-cite-global-bibliography my/bib-files)
  )

(use-package org-ref)

;;(use-package org-roam-bibtex)

(setq
 org-startup-with-latex-preview t
 )
