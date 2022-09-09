(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" default))
 '(git-gutter:added-sign "+")
 '(git-gutter:deleted-sign "-")
 '(git-gutter:modified-sign "*")
 '(package-selected-packages
   '(git-gutter tree-sitter yasnippet use-package rustic lsp-ui flycheck doom-themes company centaur-tabs all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(proof-locked-face ((t (:extend t :background "#228f36")))))

(setq visible-bell 1)

(setq-default show-trailing-whitespace t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package exec-path-from-shell
  :ensure
  :config
  (exec-path-from-shell-copy-env "PATH"))

(use-package avy
  :ensure
  :config)

(global-set-key (kbd "M-s") 'avy-goto-char-2)

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable t))

(use-package flycheck :ensure)

(use-package tree-sitter :ensure)

(use-package proof-general
  :ensure
  :config
  (setq proof-splash-enable nil)
  (setq overlay-arrow-string ""))

(use-package company-coq
  :ensure
  :config
  (add-hook 'coq-mode-hook #'company-coq-mode)
  (setq company-coq-features/prettify-symbols-in-terminals t))

(use-package yaml-mode :ensure)

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-gruvbox))

(use-package git-gutter
  :ensure
  :config
  (global-git-gutter-mode t)
  (custom-set-variables
   '(git-gutter:modified-sign "*")
   '(git-gutter:added-sign "+")
   '(git-gutter:deleted-sign "-"))
  )

; Configure backups
(setq
   backup-by-copying t
   backup-directory-alist
    '(("" . "~/.saves/"))
   delete-old-versions nil
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   vc-make-backup-files t)

(xterm-mouse-mode 1)

(global-hl-line-mode 1)
(set-face-attribute hl-line-face nil :underline t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 2))

(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 2))

(global-set-key (kbd "C-<up>") 'gcm-scroll-up)
(global-set-key (kbd "C-<down>") 'gcm-scroll-down)
(global-set-key (kbd "<mouse-4>") 'gcm-scroll-up)
(global-set-key (kbd "<mouse-5>") 'gcm-scroll-down)
