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
(setq package-archives `(("melpa" . "http://melpa.org/packages/")))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(defvar my-initflag 0)
(defvar my-package '(zenburn-theme verilog-mode web-mode auto-complete flycheck go-mode badwolf-theme basic-theme nyan-mode flycheck-pos-tip c-eldoc ac-php vimrc-mode nlinum nlinum-relative undo-tree anzu sql-indent geben multiple-cursors json-mode planet-theme indent-guide smooth-scrolling helm helm-gtags yaml-mode wgrep))
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

;; (require 'helm-config)
;; (require 'helm-gtags)
;; (helm-mode 1)
;; (global-set-key (kbd "M-x") 'helm-M-x)

(setq nyan-bar-length 16)
(nyan-mode)
(nyan-start-animation)

;; elisp
(flycheck-define-checker emacs-lisp
  "An Emacs Lisp syntax checker using the Emacs Lisp Byte compiler.

See Info Node `(elisp)Byte Compilation'."
  :command ("emacs" (eval flycheck-emacs-args)
            (eval
             (let ((path (pcase flycheck-emacs-lisp-load-path
                           (`inherit load-path)
                           (p (seq-map #'expand-file-name p)))))
               (flycheck-prepend-with-option "--directory" path)))
            (option "--eval" flycheck-emacs-lisp-package-user-dir nil
                    flycheck-option-emacs-lisp-package-user-dir)
            (option "--eval" flycheck-emacs-lisp-initialize-packages nil
                    flycheck-option-emacs-lisp-package-initialize)
            (option "--eval" flycheck-emacs-lisp-check-declare nil
                    flycheck-option-emacs-lisp-check-declare)
            "--eval" (eval (flycheck-emacs-lisp-bytecomp-config-form))
            "--eval" (eval flycheck-emacs-lisp-check-form)
            "--"
            source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":"
          (zero-or-more whitespace) "Error:" (zero-or-more whitespace)
          (message (zero-or-more not-newline)
                   (zero-or-more "\n    " (zero-or-more not-newline)))
          line-end)
   (warning line-start (file-name) ":" line ":" column ":"
            (zero-or-more whitespace) "Warning:" (zero-or-more whitespace)
            (message (zero-or-more not-newline)
                     (zero-or-more "\n    " (zero-or-more not-newline)))
            line-end)
   (warning line-start (file-name) ":" line (optional ":" column) ":"
            (zero-or-more whitespace) "Warning (check-declare): said\n"
            (message (zero-or-more "    " (zero-or-more not-newline))
                     (zero-or-more "\n    " (zero-or-more not-newline)))
            line-end)
   ;; The following is for Emacs 24 ‘check-declare-file’, which uses a
   ;; less informative format.
   (warning line-start "Warning (check-declare): " (file-name) " said "
            (message (zero-or-more not-newline))
            line-end))
  :error-filter
  (lambda (errors)
    (dolist (error errors)
      (if (or (string-match "assignment to free variable" (flycheck-error-message error))
              (string-match "reference to free variable" (flycheck-error-message error)))
        ;; Flycheck ignores errors without line numbers, but the error
        ;; message about multiple packages in a directory doesn't come with a
        ;; line number, so inject a fake one.
        (setf (flycheck-error-line error) nil)
        )
      )

    (flycheck-collapse-error-message-whitespace
     (flycheck-sanitize-errors errors))
    )
  :modes (emacs-lisp-mode lisp-interaction-mode)
  :enabled flycheck--emacs-lisp-enabled-p
  :predicate
  (lambda ()
    ;; Do not check buffers that should not be byte-compiled.  The checker
    ;; process will refuse to compile these, which would confuse Flycheck
    (not (bound-and-true-p no-byte-compile)))
  :next-checkers (emacs-lisp-checkdoc))

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

  (defun web-mode-comment (pos)
    (let (ctx language col sel beg end tmp block-side single-line-block pos-after content)

      (setq pos-after pos)

      (setq block-side (get-text-property pos 'block-side))
      (setq single-line-block (web-mode-is-single-line-block pos))

      (cond

       ((and block-side (string= web-mode-engine "erb"))
        (web-mode-comment-erb-block pos)
        )

       ((and block-side (string= web-mode-engine "artanis"))
        (web-mode-comment-artanis-block pos)
        )

       ((and single-line-block block-side
             (intern-soft (concat "web-mode-comment-" web-mode-engine "-block")))
        (funcall (intern (concat "web-mode-comment-" web-mode-engine "-block")) pos)
        )

       (t
        (setq ctx (web-mode-point-context
                   (if mark-active (region-beginning) (line-beginning-position))))
        ;;(message "%S" ctx)
        (setq language (plist-get ctx :language))
        (setq col (current-column))
        (cond
         (mark-active
          ;;(message "%S %S" (point) col)
          )
         ((and (member language '("html" "xml"))
               (get-text-property (progn (back-to-indentation) (point)) 'tag-beg))
          (web-mode-element-select))
         (t
          (end-of-line)
          (set-mark (line-beginning-position)))
         ) ;cond

        (setq beg (region-beginning)
              end (region-end))

        (when (> (point) (mark))
          (exchange-point-and-mark))

        (if (and (eq (char-before end) ?\n)
                 (not (eq (char-after end) ?\n)))
            (setq end (1- end)))

        (setq sel (buffer-substring-no-properties beg end))

        (cond

         ((member language '("html" "xml"))
          (cond
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "django"))
            (setq content (concat "{# " sel " #}")))
           ((and (= web-mode-comment-style 2) (member web-mode-engine '("ejs" "erb")))
            (setq content (concat "<%# " sel " %>")))
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "artanis"))
            (setq content (concat "<%; " sel " %>")))
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "aspx"))
            (setq content (concat "<%-- " sel " --%>")))
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "smarty"))
            (setq content (concat "{* " sel " *}")))
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "expressionengine"))
            (setq content (concat "{!-- " sel " --}")))
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "xoops"))
            (setq content (concat "<{* " sel " *}>")))
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "hero"))
            (setq content (concat "<%# " sel " %>")))
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "blade"))
            (setq content (concat "{{-- " sel " --}}")))
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "ctemplate"))
            (setq content (concat "{{!-- " sel " --}}")))
           ((and (= web-mode-comment-style 2) (string= web-mode-engine "razor"))
            (setq content (concat "@* " sel " *@")))
           (t
            (setq content (concat "<!-- " sel " -->"))
            (when (< (length sel) 1)
              (search-backward " -->")
              (setq pos-after nil))
            ))
          ) ;case html

         ((member language '("php" "javascript" "typescript" "java" "jsx"))
          (let (alt)
            (setq alt (cdr (assoc language web-mode-comment-formats)))
            ;;(message "language=%S alt=%S sel=%S col=%S" language alt sel col)
            (cond
             ((and alt (string= alt "//"))
              (setq content (replace-regexp-in-string (concat "\n[ ]\\{" (number-to-string col) "\\}") "\n" sel))
              (setq content (replace-regexp-in-string (concat "\n") "\n// " content))
              (setq content (concat "// " content)))
             ((get-text-property pos 'jsx-depth)
              (setq content (concat "{/* " sel " */}")))
             (web-mode-comment-prefixing
              (cond
               ((and alt (string= alt "/**"))
                (setq content (replace-regexp-in-string (concat "\n[ ]\\{" (number-to-string col) "\\}") "\n* " sel))
                (setq content (replace-regexp-in-string (concat "[ ]+$") "" content))
                (setq content (concat "/**\n* " content "\n */")))
               (t
                (setq content (replace-regexp-in-string (concat "\n[ ]\\{" (number-to-string col) "\\}") "\n* " sel))
                (setq content (concat "/* " content " */")))
               )
              )
             (t
              (setq content (concat "/* " sel " */")))
             ) ;cond
            ) ;let
          )

         ((member language '("erb"))
          (setq content (replace-regexp-in-string "^[ ]*" "#" sel)))

         ((member language '("asp"))
          (setq content (replace-regexp-in-string "^[ ]*" "''" sel)))

         (t
          (setq content (concat "/* " sel " */")))

         ) ;cond

        (when content
          (delete-region beg end)
          (deactivate-mark)
          (let (beg end)
            (setq beg (point-at-bol))
            (insert content)
            (setq end (point-at-eol))
            (indent-region beg end)
            )
          ) ;when

        ) ;t
       ) ;cond

      (when pos-after (goto-char pos-after))

      ))

  (defun web-mode-uncomment (pos)
    (let ((beg pos) (end pos) (sub2 "") (sub3 "") comment boundaries)
      (save-excursion
        (cond
         ((and (get-text-property pos 'block-side)
               (intern-soft (concat "web-mode-uncomment-" web-mode-engine "-block")))
          (funcall (intern (concat "web-mode-uncomment-" web-mode-engine "-block")) pos))
         ((and (setq boundaries (web-mode-comment-boundaries pos))
               (setq beg (car boundaries))
               (setq end (1+ (cdr boundaries)))
               (> (- end beg) 4))
          (when (and (eq (get-text-property beg 'part-token) 'comment)
                     (> beg 1) ;#1158
                     (get-text-property (1- beg) 'jsx-beg))
            (setq beg (1- beg)
                  end (1+ end)))
          (setq comment (buffer-substring-no-properties beg end))
          (setq sub2 (substring comment 0 2))
          (cond
           ((member sub2 '("<!" "<%"))
            (setq comment (replace-regexp-in-string "\\(^<[!%]--[ ]?\\|[ ]?--[%]?>$\\)" "" comment)))
           ((string= sub2 "{#")
            (setq comment (replace-regexp-in-string "\\(^{#[ ]?\\|[ ]?#}$\\)" "" comment)))
           ((string= sub2 "{/") ;jsx comments
            (setq comment (replace-regexp-in-string "\\(^{/\\*[ ]?\\|[ ]?\\*/}$\\)" "" comment)))
           ((string= sub2 "/*")
            (setq sub3 (substring comment 0 3))
            ;; (message "%S" comment)
            (when (string= sub3 "/**")
              (setq comment (replace-regexp-in-string "\\(^/\\*\\*\n\\)" "" comment))
              (setq comment (replace-regexp-in-string "\\(^[ \t]*\\*/\n\\)" "" comment))
              )
            ;;(message "%S" comment)
            ;;(setq comment (replace-regexp-in-string "\\(\\*/\\|^/\\*[ ]?\\|^[ \t]*\\*\\)" "" comment))
            (setq comment (replace-regexp-in-string "\\([ ]?\\*/$\\|^/\\*[ ]?\\)" "" comment))
            (setq comment (replace-regexp-in-string "\\(^[ \t]*\\*\\)" "" comment))
            ;;(message "%S" comment)
            )
           ((string= sub2 "''")
            (setq comment (replace-regexp-in-string "''" "" comment)))
           ((string= sub2 "//")
            (setq comment (replace-regexp-in-string "^ *//" "" comment)))
           ) ;cond
          (delete-region beg end)
          (web-mode-insert-and-indent comment)
          (goto-char beg)
          )
         ) ;cond
        (indent-according-to-mode)
        )))

  (hs-minor-mode 1)
  (when (equal web-mode-engine "php")
    (setq flycheck-phpcs-standard "PSR12")
    (flycheck-add-next-checker 'php 'php-phpcs)
    (flycheck-add-mode 'php 'web-mode)
    (flycheck-add-mode 'php-phpcs 'web-mode)

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

(flycheck-define-checker c/c++-gcc-2
  "A C/C++ syntax checker using GCC.

Requires GCC 4.8 or newer.  See URL `https://gcc.gnu.org/'."
  :command ("gcc"
            "-I../templates"
            "-fshow-column"
            ;; "-fno-diagnostics-show-caret" ; Do not visually indicate the source location
            "-fno-diagnostics-show-option" ; Do not show the corresponding
                                        ; warning group
            "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
            (option "-std=" flycheck-gcc-language-standard concat)
            (option-flag "-pedanti86ekc" flycheck-gcc-pedantic)
            (option-flag "-pedantic-errors" flycheck-gcc-pedantic-errors)
            (option-flag "-fno-exceptions" flycheck-gcc-no-exceptions)
            (option-flag "-fno-rtti" flycheck-gcc-no-rtti)
            (option-flag "-fopenmp" flycheck-gcc-openmp)
            (option-list "-include" flycheck-gcc-includes)
            (option-list "-W" flycheck-gcc-warnings concat)
            (option-list "-D" flycheck-gcc-definitions concat)
            (option-list "-I" flycheck-gcc-include-path)
            (eval flycheck-gcc-args)
            "-x" (eval
                  (pcase major-mode
                    (`c++-mode "c++")
                    (`c-mode "c")))
            ;; GCC performs full checking only when actually compiling, so
            ;; `-fsyntax-only' is not enough. Just let it generate assembly
            ;; code.
            "-S" "-o" null-device
            ;; Read from standard input
            "-")
  :standard-input t
  :error-patterns
  ((error line-start
          (message "In file included from") " " (or "<stdin>" (file-name))
          ":" line ":" column ":" line-end)
   (info line-start (or "<stdin>" (file-name)) ":" line ":" column
         ": note: " (message) line-end)
   (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
            ": warning: " (message) line-end)
   (error line-start (or "<stdin>" (file-name)) ":" line ":" column
          ": " (or "fatal error" "error") ": " (message) line-end))
  :error-filter
  (lambda (errors)
    (flycheck-fold-include-levels (flycheck-sanitize-errors errors)
                                  "In file included from"))
  :modes (c-mode c++-mode)
  :next-checkers ((warning . c/c++-cppcheck)))

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
