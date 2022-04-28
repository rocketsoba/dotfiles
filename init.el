;;; package --- Summary
;;; Commentary:
;;; Code:


;; backup
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup/")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backup/" t)))


;; package
;; minimum version: 25.2
(require 'package)
(if (version< emacs-version "26.2")
    (setq package-check-signature nil))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(defvar my-package
  '(
    ac-php
    anzu
    auto-complete
    badwolf-theme
    basic-theme
    c-eldoc
    flycheck
    flycheck-phpstan
    flycheck-pos-tip
    geben
    helm
    helm-gtags
    indent-guide
    json-mode
    multiple-cursors
    nlinum
    nlinum-relative
    nyan-mode
    planet-theme
    smooth-scrolling
    undo-tree
    vimrc-mode
    web-mode
    wgrep
    yaml-mode
    zenburn-theme
    )
  )
(defvar my-initflag 0)

(dolist (package my-package)
  (when (not (package-installed-p package))
      (when (equal my-initflag 0)
        (package-refresh-contents)
        (setq my-initflag 1)
        )
      (package-install package)
      )
  )


;; load function path
(add-to-list 'load-path "~/.emacs.d/lisp")


;; c
(add-hook 'c-mode-common-hook
          (lambda ()
            (load "gcc-2.el" t)
            (add-to-list 'flycheck-checkers 'c/c++-gcc-2)
            (flycheck-select-checker 'c/c++-gcc-2)
            (c-turn-on-eldoc-mode)
            )
          )


;; web-mode
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 4)
            (setq web-mode-block-padding 0)
            (setq web-mode-comment-style 2)

            (setq-default web-mode-comment-formats (delete '("php" . "/*") web-mode-comment-formats))
            (add-to-list 'web-mode-comment-formats '("php" . "/**"))

            (load "web-mode-comment" t)
            (load "web-mode-uncomment" t)

            (hs-minor-mode 1)
            (when (equal web-mode-engine "php")
              (require 'flycheck-phpstan)
              (setq flycheck-phpcs-standard "PSR12")
              (setq phpstan-level 0)
              (flycheck-add-next-checker 'php 'php-phpcs)
              (flycheck-add-next-checker 'php-phpcs 'phpstan)
              (flycheck-add-mode 'php 'web-mode)
              (flycheck-add-mode 'php-phpcs 'web-mode)
              (flycheck-add-mode 'phpstan 'web-mode)

              ;; https://github.com/xcwen/ac-php
              (require 'ac-php)
              (setq ac-sources  '(ac-source-php ))
              (yas-minor-mode 1)
              (ac-php-core-eldoc-setup)

              (setq  geben-dbgp-default-port 9001)
              )
            (when (equal (file-name-extension buffer-file-name) "ctp")
              (add-to-list 'flycheck-disabled-checkers 'php-phpcs)
              )
            (when (equal web-mode-content-type "javascript")
              (add-to-list 'flycheck-disabled-checkers 'php)
              (add-to-list 'flycheck-disabled-checkers 'php-phpcs)
              (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)
              (add-to-list 'flycheck-disabled-checkers 'javascript-jscs)
              (flycheck-add-mode 'javascript-eslint 'web-mode)
              )
            )
          )


;; elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (load "emacs-lisp" t)
            )
          )


;; auto-mode-alist
(add-to-list 'auto-mode-alist '("\\.\\([xps]html\\|html\\|tpl\\|php\\|js\\|ctp\\)\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))


;; define-key
(global-set-key   [mouse-4] '(lambda () (interactive) (scroll-down 5)))
(global-set-key   [mouse-5] '(lambda () (interactive) (scroll-up   5)))
(global-set-key [S-mouse-4] '(lambda () (interactive) (scroll-down 1)))
(global-set-key [S-mouse-5] '(lambda () (interactive) (scroll-up   1)))
(global-set-key (kbd "C-t") nil)


;; common
(column-number-mode t)
(menu-bar-mode 0)

(setq comment-style 'indent)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq-default indent-tabs-mode nil)
(setf wgrep-enable-key "e")
(setq wgrep-auto-save-buffer t)
(put 'upcase-region 'disabled nil)


;; theme
;; https://emacsthemes.com/
(cond
 ((window-system)
  (load-theme 'monokai t)
  )
 (t
  (load-theme 'badwolf t)
  (load-theme 'planet t)
  )
 )


;; nyan-mode
(require 'nyan-mode)
(setq nyan-bar-length 16)
(nyan-mode)
(nyan-start-animation)


;; auto-complete
(require 'auto-complete-config)
(ac-config-default)


;; flyheck
(require 'flycheck)
(global-flycheck-mode)
(flycheck-pos-tip-mode)


;; recentf
(require 'recentf)
(setq recentf-max-saved-items 500)
(recentf-mode 1)


;; indent-guide
(require 'indent-guide)
(indent-guide-global-mode)
;; (setq indent-guide-recursive t)


;; smooth-scrolling
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)
(smooth-scrolling-mode)


;; nlinum
(require 'nlinum)
(setq nlinum-format "%3d ")
(global-nlinum-mode t)


;; anzu
(require 'anzu)
(global-anzu-mode 1)


;; undo-tree
(require 'undo-tree)
(setq undo-tree-auto-save-history nil)
(define-key undo-tree-map (kbd "M-/") 'undo-tree-redo)
(global-undo-tree-mode t)


;; whitespace-mode
;; toggle後、読み込み直すと無効化できる
(require 'whitespace)
(setq whitespace-style
      '(
        face
        trailing
        tabs
        space-mark
        tab-mark
        )
      )
(setq whitespace-display-mappings '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
(global-whitespace-mode 1)


;; savehist
(require 'savehist)
(setq savehist-additional-variables '(extended-command-history))
(savehist-mode 1)


(provide 'init)
;;; init.el ends here
