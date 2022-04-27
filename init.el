;;; package --- Summary
;;; Commentary:
;;----"M-x eval-current-buffer" to reload init.el----
;;----"M-x describe-variable"で変数確認,"set-variable"で設定----
;;----"M-x transient-mark-mode"で選択範囲の色を出すか切り替える----
;;; Code:
(column-number-mode t)
;; (display-time)

(menu-bar-mode 0)
;; スクロールマウスの設定
(global-set-key   [mouse-4] '(lambda () (interactive) (scroll-down 5)))
(global-set-key   [mouse-5] '(lambda () (interactive) (scroll-up   5)))
;;                 Shift
(global-set-key [S-mouse-4] '(lambda () (interactive) (scroll-down 1)))
(global-set-key [S-mouse-5] '(lambda () (interactive) (scroll-up   1)))

;;----バックアップファイルの抑制---
;;----"http://www.emacswiki.org/emacs/BackupDirectory"----
;;----"http://masutaka.net---/chalow/2014-05-11-1.html"----
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)
(setq backup-directory-alist `((".*" . "~/.emacs.d/backup/")))
(setq auto-save-file-name-transforms `((".*", "~/.emacs.d/backup/" t)))

;;----MELPAレポジトリ追加(emacs24以降)----
;; minimum version: 25.2
(require 'package)
(if (version< emacs-version "26.2")
    (setq package-check-signature nil))
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(defvar my-initflag 0)
(defvar my-package '(zenburn-theme verilog-mode web-mode auto-complete flycheck badwolf-theme basic-theme nyan-mode flycheck-pos-tip c-eldoc ac-php vimrc-mode nlinum nlinum-relative undo-tree anzu sql-indent geben multiple-cursors json-mode planet-theme indent-guide smooth-scrolling helm helm-gtags yaml-mode wgrep flycheck-phpstan))
(dolist (package my-package)
  (unless (package-installed-p package)
    (progn
      (if (equal my-initflag 0)
          (package-refresh-contents)
        )
      (setq my-initflag 1)
      (package-install package)
      )
    )
  )

;;----lispのパスを通す----
(add-to-list 'load-path "~/.emacs.d/lisp")

;; (require 'helm-config)
;; (require 'helm-gtags)
;; (helm-mode 1)
;; (global-set-key (kbd "M-x") 'helm-M-x)

(setq nyan-bar-length 16)
(nyan-mode)
(nyan-start-animation)

;; elisp
(load "emacs-lisp")

;;----web-mode----
;;----"http://yanmoo.blogspot.jp/2013/06/html5web-mode.html"----
(add-to-list 'auto-mode-alist '("\\.\\([xps]html\\|html\\|tpl\\|php\\|js\\|ctp\\)\\'" . web-mode))
(setq-default indent-tabs-mode nil)
(autoload 'web-mode "web-mode" nil t)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  ;;----htmlのインデント----
  (setq web-mode-markup-indent-offset 2)
  ;;----CSSのインデント----
  (setq web-mode-css-indent-offset 2)
  ;;----PHP,JSなどのインデント----
  (setq web-mode-code-indent-offset 4)
  ;;----<?phpのしたのインデント----
  (setq web-mode-block-padding 0)
  ;;----コメントのスタイル----
  (setq web-mode-comment-style 2)

  (setq-default web-mode-comment-formats (delete '("php" . "/*") web-mode-comment-formats))
  (add-to-list 'web-mode-comment-formats '("php" . "/**"))

  (load "web-mode-comment")
  (load "web-mode-uncomment")

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
    (auto-complete-mode t)
    (require 'ac-php)
    (setq ac-sources  '(ac-source-php ) )
    (yas-global-mode 1)
    (ac-php-core-eldoc-setup)
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
(add-hook 'web-mode-hook  'my-web-mode-hook)

;;----theme----
;;----"https://emacsthemes.com/"----
(cond
 ((window-system)
  (load-theme 'monokai t)
  )
 (t
  (load-theme 'badwolf t)
  (load-theme 'planet t)
  )
 )

;;----auto-complete.el----
;;----"http://fukuyama.co/emacs-auto-complete"----
;;----別ファイルなしでもdict読み込めるかもしれない----
;; (require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/dict")
(ac-config-default)

;;----flyheck----
(global-flycheck-mode)
(flycheck-pos-tip-mode)

(load "gcc-2.el")

(add-to-list 'flycheck-checkers 'c/c++-gcc-2)

(add-hook 'c-mode-common-hook
          (lambda ()
            (flycheck-select-checker 'c/c++-gcc-2)
            (flycheck-mode)
            (c-turn-on-eldoc-mode)
            ))

(setq comment-style 'indent)

;;----ファイル重複時にDIR表示----
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;----C-x C-fで履歴有効化----
(require 'recentf)
(setq recentf-max-saved-items 500)
(recentf-mode +1)

;; indent
(require 'indent-guide)
(indent-guide-global-mode)
;; (setq indent-guide-recursive t)

;; スムーズスクロール
(smooth-scrolling-mode)
(setq smooth-scroll-margin 5)

;; nlinum
(global-nlinum-mode t)
(setq nlinum-format "%3d ")


(global-anzu-mode +1)

(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; (eval-after-load "sql"
;;   (load-library "sql-indent"))

(setq  geben-dbgp-default-port 9001)

;; whitespace-mode
(require 'whitespace)
(setq whitespace-style '(face
                         trailing
                         tabs
                         space-mark
                         tab-mark))
(setq whitespace-display-mappings
      '((tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
(global-whitespace-mode 1)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
;; (require 'command-log-mode)
;; (global-command-log-mode)

(savehist-mode 1)
(setq savehist-additional-variables '(extended-command-history))

;;----x関連をwithでビルドすると動きそう----
;; (require 'simpleclip)
;; (xterm-mouse-mode t)
;; (simpleclip-mode t)

(put 'upcase-region 'disabled nil)

;; wgrep
(setf wgrep-enable-key "e")
(setq wgrep-auto-save-buffer t)

(provide 'init)
;;; init.el ends here
