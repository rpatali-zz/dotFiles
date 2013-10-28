;; Disable menubar, toolbar, scrollbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; elpa, marmalade
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)


;; solarized, installed using packages
(load-theme 'solarized-light t)


;; install smex from packages, same as ido but for command names
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; install markdown-mode using packages
;; markdown mode automatically load for .markdown, .md
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; install auto-complete from packages and load it always
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)


;; vim-easymotion like plugin, install ace-jump-mode using packages
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)


;; install jedi using packages
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)


;; install ag using packages
;; setup color highlighting using this option
(require 'ag)
(setq ag-highlight-search t)
;; reuse same buffer for all your ag searches
(setq ag-reuse-buffers 't)

;; install magit using packages, git from emacs
(require 'magit)


;; install git-gutter using packages
(require 'git-gutter)
(global-git-gutter-mode +1)


;; install flx-ido using packages
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)


;; install projectile using packages
(require 'projectile)
;; enable projectile globally
(projectile-global-mode)


;; install helm using packages
(require 'helm)
(require 'helm-projectile)
(require 'helm-config)
;; auto load helm
(helm-mode 1)
;; bind helm to C-c h 
(global-set-key (kbd "C-c h") 'helm-projectile)

