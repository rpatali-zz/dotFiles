;; Disable menubar, toolbar, scrollbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; recentf loaded on start
(recentf-mode 1)
;; ido-mode for better file browsing
(ido-mode t)


;; elpa, marmalade
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)


;; solarized, installed using packages
(load-theme 'solarized-light t)


;; install smex from packages, same as ido but for command names
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; install markdown-mode using packages
;; markdown mode automatically load for .markdown, .md
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; install auto-complete from packages and load it always
(require 'auto-complete-config)
(ac-config-default)


;; vim-easymotion like plugin, install ace-jump-mode using packages
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


;; install jedi using packages
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)


;; install ag using packages
;; setup color highlighting using this option
(setq ag-highlight-search t)
;; reuse same buffer for all your ag searches
(setq ag-reuse-buffers 't)


;; install git-gutter using packages
(global-git-gutter-mode +1)
