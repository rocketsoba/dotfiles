;;----"M-x eval-current-buffer" to reload init.el----
;;----"M-x describe-variable"で変数確認,"set-variable"で設定----

(column-number-mode t)
(global-linum-mode t)
;;----"M-x linum-mode"で切り替え----x
(display-time)
(setq display-time-day-and-date t)
(show-paren-mode 1)
(setq confirm-kill-emacs nil)
;; (menu-bar-mode 0)
;; スクロールマウスの設定
;; (global-set-key   [mouse-4] '(lambda () (interactive) (scroll-down 5)))
;; (global-set-key   [mouse-5] '(lambda () (interactive) (scroll-up   5)))
;; ;;                 Shift
;; (global-set-key [S-mouse-4] '(lambda () (interactive) (scroll-down 1)))
;; (global-set-key [S-mouse-5] '(lambda () (interactive) (scroll-up   1)))
;;----lispのパスを通す----
(add-to-list 'load-path "~/.emacs.d/lisp")

;;----バックアップファイルの抑制---
;;----"http://www.emacswiki.org/emacs/BackupDirectory"----
;;----"http://masutaka.net---/chalow/2014-05-11-1.html"----
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)
(setq backup-directory-alist `((".*" . "~/.emacs.d/backup/")))
(setq auto-save-file-name-transforms `((".*", "~/.emacs.d/backup/" t)))

;;----MELPAレポジトリ追加(emacs24以降)----
(require 'package)
(setq package-archives `(("melpa" . "http://stable.melpa.org/packages/")))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "http://stable.melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents) 
;;----(if (not (require 'hoge nil t)) (任意))はmustっぽい----
;; (if (not (require 'flycheck nil t))
;;     (package-refresh-contents) 
;;   )
(setq refresh-require 0)
(defvar my-package '(zenburn-theme web-mode auto-complete))
(dolist (package my-package)
  (unless (package-installed-p package)
    (if (equal refresh-require 0)
	(package-refresh-contents))
    (setq refresh-require 1)
    (package-install package)))

;;----web-mode----
;;----"http://yanmoo.blogspot.jp/2013/06/html5web-mode.html"----
(add-to-list 'auto-mode-alist '("\\.\\([xps]html\\|html\\|tpl\\|php\\|js\\)\\'" . web-mode))
(autoload 'web-mode "web-mode" nil t)
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  ;;----htmlのインデント----
  (setq web-mode-markup-indent-offset 2) 
  ;;----CSSのインデント----
  (setq web-mode-css-indent-offset 2)
  ;;----PHP,JSなどのインデント----
  (setq web-mode-code-indent-offset 2)
  ;;----<?phpのしたのインデント----
  (setq web-mode-block-padding 2)
  ;;----コメントのスタイル----
  (setq web-mode-comment-style 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;;----themeをzenburnに----
;;----他のtheme----
;;----"https://emacsthemes.com/"----
(load-theme 'zenburn t)

;;----auto-complete.el----
;;----"http://fukuyama.co/emacs-auto-complete"----
;;----別ファイルなしでもdict読み込めるかもしれない----
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-1.5.1/dict")
(ac-config-default)
(add-to-list 'ac-modes 'web-mode) 

;;----flyheck----
;;----まだあまりわかってない----
;; (package-install 'flycheck)
;; (global-flycheck-mode)
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;;----ファイル重複時にDIR表示----
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) 

;;----C-x C-fで履歴有効化----
(require 'recentf)
(setq recentf-max-saved-items 500)
(recentf-mode +1)

;;----x関連をwithでビルドすると動きそう----
;; (require 'simpleclip)
;; (xterm-mouse-mode t)
;; (simpleclip-mode t)

;;---el-get.el----
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))
;; (require 'el-get)
;; (add-to-list 'el-get-recipe-path "~/.emacs.d/elpa")
;; (el-get 'sync)
;;----el-getによってgit,emacswiki,ELPAなどからpackageがとってこれる----
;;----が、各package設定がよくわからないし管理もわからなかったのでそのうちやる----
;;----git管理&無意味なディレクトリ構造がなくなってよさそう----
;;----not without-gnutlsでビルドが必要----
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default")))))
;; (add-to-list 'default-frame-alist '(alpha . (0.90 0.90)))
