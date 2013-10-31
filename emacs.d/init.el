;;; init.el --- this is where all the magic happens

;;; Commentary:
;;    packages are all downloaded using M-x p-l-p

;;; Code:

;; disable menubar, toolbar, scrollbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


(require 'recentf) ; {

    (recentf-mode 1)
    (setq recentf-max-menu-items 25)
    (global-set-key "\C-x\ \C-r" 'recentf-open-files)

; }



;; elpa, marmalade
(require 'package) ; {

    (dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                      ("melpa" . "http://melpa.milkbox.net/packages/")))
            (add-to-list 'package-archives source t))
    (package-initialize)

; }


(require 'color-theme) ; {

    (require 'color-theme-solarized) ; {
        (load-theme 'solarized-light t)
    ; }
; }


(require 'smex) ; {

    (smex-initialize)
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
    ;; This is your old M-x.
    (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

; }


;; markdown mode automatically load for .markdown, .md
(require 'markdown-mode) ; {

    (autoload 'markdown-mode "markdown-mode"
      "Major mode for editing Markdown files" t)
     (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
     (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; }


;; install auto-complete from packages and load it always
(require 'auto-complete) ; {

    (require 'auto-complete-config) ; {
        (ac-config-default)
    ; }
; }


(require 'ace-jump-mode) ; {

    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

; }


(require 'jedi) ; {

    (add-hook 'python-mode-hook 'jedi:setup)
    (setq jedi:setup-keys t)

; }


(require 'ag) ; {

    (setq ag-highlight-search t)
    ;; reuse same buffer for all your ag searches
    (setq ag-reuse-buffers 't)

; }


(require 'magit) ; {

; }


(require 'git-gutter) ; {

    (global-git-gutter-mode +1)

; }


(require 'flx-ido) ; {

    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    ;; disable ido faces to see flx highlights.
    (setq ido-use-faces nil)

;}


(require 'flycheck) ; {

    ;; enable flycheck in all buffers where applicable
    (add-hook 'after-init-hook #'global-flycheck-mode)

; }


(require 'expand-region) ; {

    (global-set-key (kbd "C-=") 'er/expand-region)

; }


(require 'multiple-cursors) ; {

    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

; }


(require 'autopair) ; {

    (autopair-global-mode)

; }


;; haskell stuff
(require 'haskell-mode) ; {

    (require 'flymake-haskell-multi) ; {
        (add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)
    ; }

    (require 'flymake-hlint) ; {
        (add-hook 'haskell-mode-hook 'flymake-hlint-load)
    ; }

; }


(require 'rainbow-delimiters) ; {

    (global-rainbow-delimiters-mode)

; }


(require 'ctags) ; {

    (setq tags-revert-without-query t)
    (global-set-key (kbd "C-.") 'ctags-create-or-update-tags-table)
    (global-set-key (kbd "M-.")  'ctags-search)

; }


(provide 'init)
;;; init.el ends here
