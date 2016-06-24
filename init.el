;;; package --- Summary
;;; Commentary:
;;----"M-x eval-current-buffer" to reload init.el----
;;----"M-x describe-variable"で変数確認,"set-variable"で設定----
;;; Code:
(column-number-mode t)
(global-linum-mode t)
;;----"M-x linum-mode"で切り替え----x
(display-time)
(setq display-time-day-and-date t)
(show-paren-mode 1)
(setq confirm-kill-emacs nil)
(setq default-frame-alist '((width . 80) (height . 30)))
;; Mac環境でnw,GUIともになぜか動作しない
(cond 
 ((equal system-type 'darwin) 
  (cond
   ((window-system)

    (set-face-attribute 'default nil
			:family "monaco" ;; font
			:height 150)     ;; font size
    (set-fontset-font
     (frame-parameter nil 'font)
     'japanese-jisx0208
     '("Hiragino Maru Gothic ProN" . "iso10646-1"))
    (setq initial-frame-alist
	  '((width . 80) (height . 40)))
    ))
  ))
(cond 
 ((equal system-type 'usg-unix-v)
  ;; (cond
  ;;  ((featurep 'x-toolkit)
  ;; (custom-set-faces '(default ((t (:family "unknown-DejaVu Sans" :height 13 :weight normal)))))
  ;; (add-to-list 'default-frame-alist
  ;; 	       '(font . "helvetica:size=13:weight=bold"))
  ;; (create-fontset-from-ascii-font "DejaVu Sans Mono-12:weight=normal" nil "Dejavu")
  ;; (set-face-attribute 'default nil
  ;; 		      :family "DejaVu Sans Mono" :height 140)
  ;; (set-fontset-font  (frame-parameter nil 'font)
  ;; 		     'japanese-jisx0208
  ;; 		     (font-spec :family "DejaVu Sans Mono"))
  ;; (custom-set-faces '(default ((t (:family "fixed" :foundry "sony" :slant normal :weight normal :height 128 :width normal)))))
  (setq initial-frame-alist
	'((width . 100) (height . 50)))
  (custom-set-variables
   '(default ((t (:inherit nil :stipple nil :background "unspecified-bg" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default")))))
  ))
;; (setq inhibit-startup-message t)
(menu-bar-mode 0)
;; スクロールマウスの設定
(global-set-key   [mouse-4] '(lambda () (interactive) (scroll-down 5)))
(global-set-key   [mouse-5] '(lambda () (interactive) (scroll-up   5)))
;;                 Shift
(global-set-key [S-mouse-4] '(lambda () (interactive) (scroll-down 1)))
(global-set-key [S-mouse-5] '(lambda () (interactive) (scroll-up   1)))
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
(defvar my-package '(zenburn-theme verilog-mode web-mode auto-complete flycheck))
(dolist (package my-package)
  (unless (package-installed-p package)
    (package-install package)))

(require 'nyan-mode)
(setq nyan-bar-length 8)
(nyan-mode)
(nyan-start-animation)

;;----web-mode----
;;----"http://yanmoo.blogspot.jp/2013/06/html5web-mode.html"----
(add-to-list 'auto-mode-alist '("\\.\\([xps]html\\|html\\|tpl\\|php\\|js\\)\\'" . web-mode))
;; (add-to-list 'web-mode-content-types' ("php" . "\\.php\\'")) 
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
  ;; (setq web-mode-php-indent-offset 2) 
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)
;; (eval-after-load "web-mode"
;;   '(add-to-list 'web-mode-content-types' ("php" . "\\.php\\'")))

;;----themeをzenburnに----
;;----他のtheme----
;;----"https://emacsthemes.com/"----
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(cond 
 ((window-system)
  (load-theme 'monokai t)
  )
 (t
  (load-theme 'zenburn t)
  )
 )

;;----auto-complete.el----
;;----"http://fukuyama.co/emacs-auto-complete"----
;;----別ファイルなしでもdict読み込めるかもしれない----
(require 'auto-complete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/dict")
;; (defvar web-mode-ac-sources-alist
;;   '(("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
;;     ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))
(ac-config-default)
;; (add-hook 'web-mode-before-auto-complete-hooks
;; 	  '(lambda ()
;; 	     (let ((web-mode-cur-language
;; 		    (web-mode-language-at-pos)))
;; 	       (if (string= web-mode-cur-language "php")
;; 		   (yas-activate-extra-mode 'php-mode)
;; 		 (yas-deactivate-extra-mode 'php-mode))
;; 	       (if (string= web-mode-cur-language "css")
;; 		   (setq emmet-use-css-transform t)
;; 		 (setq emmet-use-css-transform nil)))))
;; (add-to-list 'ac-modes 'web-mode) 

;;----flyheck----
;;----まだあまりわかってない----
;; (package-install 'flycheck)
(global-flycheck-mode)
;; (add-hooautocok 'after-init-hook #'global-flycheck-mode)
(flycheck-define-checker web-mode-php
  "A PHP syntax checker using the PHP command line interpreter.

See URL `http://php.net/manual/en/features.commandline.php'."
  :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
            "-d" "log_errors=0")
  :error-patterns
  ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
          (message) " in - on line " line line-end))
  :modes (web-mode)
  )

(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-engine "php")
              ;; enable flycheck
              (flycheck-select-checker 'web-mode-php)
              (flycheck-mode))))

;;----ファイル重複時にDIR表示----
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) 

;;----C-x C-fで履歴有効化----
(require 'recentf)
(setq recentf-max-saved-items 500)
(recentf-mode +1)

;;Load verilog-mode only when needed
(autoload 'verilog-mode "verilog-mode" nil t )

;; Any files that end in .v should be in verilog mode
(add-to-list 'auto-mode-alist '("\\.\\(verilog\\|template\\)\\'" . verilog-mode))

(cond 
 ((package-installed-p "anything")

  (require 'cl)  ; loop, delete-duplicates
  
  (defun anything-font-families ()
    "Preconfigured `anything' for font family."
    (interactive)
    (flet ((anything-mp-highlight-match () nil))
      (anything-other-buffer
       '(anything-c-source-font-families)
       "*anything font families*")))

  (defun anything-font-families-create-buffer ()
    (with-current-buffer
	(get-buffer-create "*Fonts*")
      (loop for family in (sort (delete-duplicates (font-family-list)) 'string<)
	    do (insert
		(propertize (concat family "\n")
			    'font-lock-face
			    (list :family family :height 2.0 :weight 'bold))))
      (font-lock-mode 1)))

  (defvar anything-c-source-font-families
    '((name . "Fonts")
      (init lambda ()
	    (unless (anything-candidate-buffer)
	      (save-window-excursion
		(anything-font-families-create-buffer))
	      (anything-candidate-buffer
	       (get-buffer "*Fonts*"))))
      (candidates-in-buffer)
      (get-line . buffer-substring)
      (action
       ("Copy Name" lambda
	(candidate)
	(kill-new candidate))
       ("Insert Name" lambda
	(candidate)
	(with-current-buffer anything-current-buffer
	  (insert candidate))))))
  ))

;; Any files in verilog mode shuold have their keywords colorized

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
