;;; Backups

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

;;; General settings

;; Disable splash screen
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; Follow symlinks
(setq vc-follow-symlinks t)

(setq visible-bell t ;; Disable sound bell
      show-trailing-whitspace t ;; Show trailing whitespaces
      )

;; Enable mouse
(xterm-mouse-mode 1)

;; Display line numbers
(add-hook 'prog-mode-hook
	  'display-line-numbers-mode)

;;; Loading packages

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

;; avy
(use-package avy
  :ensure
  :bind
  ("M-s s" . avy-goto-char)
  ("M-s M-s" . avy-goto-char-2)
  ("M-s w" . avy-goto-word-1)
  ("M-s M-w" . avy-goto-word-0)
  ("M-s q" . avy-goto-char-timer))

;; Semantic language packages/configurations
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

(use-package company
  :ensure
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package flycheck
  :ensure)

;; Rust
(use-package rustic
  :ensure
  :config
  (setq rustic-format-on-save t))

;;; Faces

(use-package darcula-theme
  :ensure
  :config
  (load-theme 'darcula t))

(use-package all-the-icons
  :ensure)

(use-package centaur-tabs
  :ensure
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

;;; Custom set variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("79586dc4eb374231af28bbc36ba0880ed8e270249b07f814b0e6555bdcb71fab" default))
 '(package-selected-packages '(exec-path-from-shell rustic use-package bind-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
