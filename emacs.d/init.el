;;; Backups ;;;
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save"))
      backup-by-copying t
      make-backup-files t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-old-versions 6
      kept-new-versions 9
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 100
      vc-make-backup-files t)

(defun force-backup-of-buffer ()
  (when (not buffer-backed-up)
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)


;;; General settings ;;;
;; Disable splash screen
(setq inhibit-startup-message t
      initial-scratch-message nil)

(setq-default vc-follow-symlinks t ;; Follow symlinks
      visible-bell t ;; Disable sound bell
      show-trailing-whitespace t ;; Show trailing whitespaces
      )

;; Save history
(use-package savehist
  :init
  (savehist-mode t))

;; Normal selection by default
(delete-selection-mode 1)

;; Enable mouse
(xterm-mouse-mode 1)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

;; Disable menu-bar
(menu-bar-mode -1)

;; Enable mouse
(xterm-mouse-mode 1)

;; Display line numbers
(add-hook 'prog-mode-hook
	  'display-line-numbers-mode)


;;; Loading packages ;;;
;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; use-package
(unless (package-installed-p '(use-package))
  (package-install 'use-package))

;; Make sure to load the PATH from the shell
(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))


;;; Editor ;;;
;; For minibuffer completion
(use-package vertico
  :ensure
  :config
  (vertico-mode t))

;; For rich completion annotations
(use-package marginalia
  :ensure
  :init
  (marginalia-mode t))

;; Display keybindings help
(use-package which-key
  :ensure
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.75))

(use-package multiple-cursors
  :ensure
  :bind
  ("M-s l" . 'mc/edit-lines))

;; For quick motion ; replacement for `vim-sneak'
(use-package avy
  :ensure
  :bind
  ("M-s M-s" . avy-goto-char-2))

;; `orderless' completion style.
(use-package orderless
  :ensure
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package treemacs
  :ensure
  :bind
  (:map global-map
	("C-t" . treemacs)))

(set-frame-font "Iosevka 11" nil t)

(use-package gruvbox-theme
  :ensure
  :config
  (load-theme 'gruvbox-dark-medium t))

(use-package powerline
  :ensure
  :init
  (powerline-default-theme))

(use-package all-the-icons
  :ensure)


;;; Semantic language packages/configurations ;;;
(use-package lsp-mode
  :ensure
  :bind-keymap
  ("C-l" . lsp-command-map)
  :bind
  (:map lsp-command-map
	("C-r" . lsp-rename)
	("C-a" . lsp-execute-code-action)
        ("C-d" . lsp-find-definition)
	("C-s" . lsp-find-references))
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints t))

(use-package lsp-ui
  :ensure)

(use-package company
  :ensure
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package flycheck
  :ensure)

(use-package yasnippet
  :ensure
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure)

;; Nix
(use-package nix-mode
  :ensure
  :mode "\\.nix\\'")

;; Coq
(use-package proof-general
  :ensure
  :config
  (setq proof-splash-enable nil
	overlay-arrow-string ""))

(use-package company-coq
  :ensure
  :init
  (add-hook 'coq-mode-hook #'company-coq-mode)
  :config
  (setq company-coq-features/prettify-symbols-in-terminals t))

;; Rust
(use-package rustic
  :ensure
  :config
  (setq rustic-format-on-save t))

;; C & co
(use-package ccls
  :ensure)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(multiple-cursors treemacs-icons-dired treemacs yasnippet-snippets which-key vertico use-package rustic proof-general powerline orderless nix-mode marginalia lsp-ui gruvbox-theme flycheck exec-path-from-shell darcula-theme company-coq ccls avy all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
