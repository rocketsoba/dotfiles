;;; package --- Summary
;;; Commentary:
;;; Code:


;; backup
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup/")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backup/" t)))


;; custom-file
(setq custom-file "~/.emacs.d/custom.el")
(if (file-readable-p custom-file)
    (load custom-file)
  )


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
    company
    flycheck
    flycheck-phpstan
    flycheck-pos-tip
    geben
    highlight-parentheses
    indent-guide
    json-mode
    lsp-mode
    markdown-mode
    multiple-cursors
    nyan-mode
    package-utils
    planet-theme
    request
    smooth-scrolling
    undo-tree
    vimrc-mode
    web-mode
    wgrep
    yaml-mode
    yasnippet
    yasnippet-snippets
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


;; 一週間ごとにパッケージの更新を確認する
(require 'package-utils)
(setq last-update-checked-date-path "~/.emacs.d/last_update_checked_date")
(setq updated-package-list-path "~/.emacs.d/updated_package_list")

(if (file-readable-p last-update-checked-date-path)
    (with-temp-buffer
      (insert-file-contents last-update-checked-date-path)
      (let ((last-updated-time (read (buffer-substring-no-properties (point-min) (point-max))))
            (current-time (truncate (float-time))))
        (setq package-update-elapsed-time (- current-time last-updated-time))
        )
      )
  )

(when (or
       (not (boundp 'package-update-elapsed-time))
       (>= package-update-elapsed-time (* 86400 7))
       )

  (package-refresh-contents)
  (setq upgradable-packages (package-utils-upgradable-packages))

  (when upgradable-packages
    (let ((package-info-list))
      (dolist (package upgradable-packages)
        (package-utils-upgrade-by-name package)
        (add-to-list 'package-info-list (package-desc-full-name (car (cdr (assq package package-alist)))))
        )
      (setq package-update-list-element (cons (current-time-string) (list package-info-list)))
      )

    (cond
     ((file-readable-p updated-package-list-path)
      (with-temp-buffer
        (insert-file-contents updated-package-list-path)
        (setq package-update-list (read (buffer-substring-no-properties (point-min) (point-max))))
        )
      )
     (t
      (setq package-update-list ())
      )
     )

    (add-to-list 'package-update-list package-update-list-element)

    (with-temp-buffer
      (insert (prin1-to-string package-update-list))
      (write-region nil nil updated-package-list-path)
      )
    )

  (with-temp-buffer
    (insert (prin1-to-string (truncate (float-time))))
    (write-region nil nil last-update-checked-date-path)
    )
  )


;; load function path
(add-to-list 'load-path "~/.emacs.d/lisp")


;; semantic-mode is enabled only in c/c++-mode
(add-hook 'change-major-mode-hook
          (lambda ()
            (when (not (or
                        (equal major-mode 'c++-mode)
                        (equal major-mode 'c-mode)
                        ))
              (semantic-mode -1)
              )
            )
          )


;; c/c++
(require 'semantic)
(require 'auto-complete)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (not (equal buffer-file-name nil))
              (setq c-basic-offset 4)
              (setq gcc-version nil)

              (let ((gcc-info (shell-command-to-string "gcc --version")))
                (string-match "^gcc ([^)]+) \\([\\.0-9]+\\)" gcc-info)
                (setq gcc-version (match-string 1 gcc-info))
                (if (and (version< gcc-version "6.1") (version<= "4.9" gcc-version))
                    (setq flycheck-gcc-language-standard "c++14")
                  )
                )

              (if (not (member 'c/c++-gcc-2 flycheck-checkers))
                  (load "gcc-2" t)
                )
              (add-to-list 'flycheck-checkers 'c/c++-gcc-2)
              (flycheck-select-checker 'c/c++-gcc-2)


              (global-semanticdb-minor-mode)
              (global-semantic-idle-scheduler-mode)
              (global-semantic-idle-completions-mode)
              (if (not (fboundp 'ac-semantic-candidates2))
                  (load "ac-semantic-candidates2" t)
                )
              (semantic-mode 1)
              ;; function name completion is available in `ac-source-semantic-raw`
              (ac-define-source semantic-raw2
                                '((available . (or (require 'semantic-ia nil t)
                                                   (require 'semantic/ia nil t)))
                                  (candidates . (ac-semantic-candidates2 ac-prefix))
                                  (document . ac-semantic-doc)
                                  (action . ac-semantic-action)
                                  (symbol . "s")))
              (setq ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers ac-source-semantic ac-source-semantic-raw2))

              (c-turn-on-eldoc-mode)
              (let ((c-eldoc-include-path ""))
                (setq c-eldoc-include-path (mapcar (lambda (include-path)
                                                     (semantic-add-system-include include-path)
                                                     (concat "-I" include-path " ")
                                                     )
                                                   flycheck-gcc-include-path)
                      )
                (setq c-eldoc-include-path (apply (function concat) c-eldoc-include-path))
                (setq c-eldoc-includes (concat "`pkg-config gtk+-2.0 --cflags` -I./ -I../ " c-eldoc-include-path))
                )
              )
            )
          )


;; web-mode
;; PHP5.6以下の時にphpctagsのバージョンを下げる
(require 'request)
(setq ac-php-core-path (concat "~/.emacs.d/elpa/"
                               (package-desc-full-name (car (cdr (assq 'ac-php-core package-alist))))))
(let ((path-and-urls (list (cons (concat ac-php-core-path "/phpctags56") "https://github.com/xcwen/ac-php/raw/362907ca3dac0b5525a6881678e0f07b82f7a77f/phpctags")
                           (cons (concat ac-php-core-path "/phpctags70") "https://github.com/xcwen/ac-php/raw/master/phpctags"))))
  (dolist (path-and-url path-and-urls)
    (when (not (file-readable-p (car path-and-url)))
      (request (cdr path-and-url)
               :sync t
               :complete (cl-function
                          (lambda (&key response &allow-other-keys)
                            (with-temp-buffer
                              (let ((result (request-response-data response))
                                    (coding-system-for-read 'no-conversion)
                                    (coding-system-for-write 'no-conversion))
                                (insert result)
                                (write-region nil nil (car path-and-url))
                                (set-file-modes (car path-and-url) #o755)
                                )
                              )
                            )
                          )
               )
      )
    )
  )
(let ((php-version (shell-command-to-string "php -v")))
  (string-match "^PHP \\([0-9]+\\.[0-9]+\\.[0-9]+\\)" php-version)
  (cond
   ((version< (match-string 1 php-version) "7.0")
    (copy-file (concat ac-php-core-path "/phpctags56") (concat ac-php-core-path "/phpctags") t)
    )
   (t
    (copy-file (concat ac-php-core-path "/phpctags70") (concat ac-php-core-path "/phpctags") t)
    )
   )
  )
(let ((dirs (list (concat "~/.emacs.d/elpa/"
                                          (package-desc-full-name (car (cdr (assq 'yasnippet-snippets package-alist))))
                                          "/snippets/web-mode/")
                  "~/.ac-php/")
            ))
  (dolist (dir dirs)
    (when (file-exists-p dir)
      (delete-directory dir t nil)
      )
    )
  )
(require 'web-mode)
(add-hook 'web-mode-hook
          (lambda ()
            (web-mode-guess-engine-and-content-type)

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
              (setq flycheck-phpcs-standard "PSR12")
              (flycheck-add-next-checker 'php 'php-phpcs)
              (flycheck-add-mode 'php 'web-mode)
              (flycheck-add-mode 'php-phpcs 'web-mode)
              ;; (when (executable-find "phpstan")
              ;;   (require 'flycheck-phpstan)
              ;;   (setq phpstan-level 0)
              ;;   (flycheck-add-next-checker 'php-phpcs 'phpstan)
              ;;   (flycheck-add-mode 'phpstan 'web-mode)
              ;;   )

              ;; https://github.com/xcwen/ac-php
              ;; (require 'ac-php)
              ;; (load "ac-php-candidate-ac" t)
              ;; (setq ac-sources  '(ac-source-php ))
              ;; (yas-minor-mode 1)
              ;; (ac-php-core-eldoc-setup)
              ;; (yas-activate-extra-mode 'php-mode)

              (when (executable-find "intelephense")
                (auto-complete-mode -1)
                (lsp)
                )
              (yas-minor-mode 1)

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


;; sql
(require 'sql)
(add-hook 'sql-mode-hook
          (lambda ()
            (if (or (string-match "ENGINE=InnoDB" (buffer-string))
                    (string-match "ENGINE=MyISAM" (buffer-string)))
                (sql-set-product "mysql")
              )
            )
          )


;; lsp-mode
(if (version< emacs-version "28.1")
    (require 'compat)
  )
(add-hook 'lsp-mode-hook
          (lambda ()
            (setq lsp-enable-file-watchers nil)
            (setq lsp-keep-workspace-alive nil)
            (setq lsp-log-io nil)
            (setq lsp-idle-delay 0.5)
            (setq lsp-ui-doc-enable nil)
            (setq lsp-ui-sideline-show-diagnostics nil)
            (when (and
                   (fboundp 'json-available-p)
                   (fboundp 'native-comp-available-p)
                   (json-available-p)
                   (native-comp-available-p)
                   )
              (setq lsp-ui-doc-enable t)
              (setq lsp-ui-doc-show-with-cursor t)
              (setq lsp-ui-sideline-show-diagnostics t)
              (setq lsp-auto-configure t)
              )
            )
          )
(add-hook 'lsp-after-diagnostics-hook
          (lambda ()
            (flycheck-remove-next-checker 'lsp 'php)
            (when (and
                   (equal major-mode 'web-mode)
                   (equal web-mode-engine "php")
                   )
              (flycheck-add-next-checker 'lsp 'php)
              )
            )
          )
(add-hook 'lsp-ui-imenu-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            )
          )


;; auto-mode-alist
(add-to-list 'auto-mode-alist '("\\.\\([xps]html\\|html\\|tpl\\|php\\|js\\|ctp\\)\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
;; shebang
(add-to-list 'interpreter-mode-alist '("php" . web-mode))


;; define-key
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
(setq gc-cons-threshold (* 16 (* 1024 1024)))
(setq read-process-output-max (* 1024 1024))


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


;; display-line-numbers
(require 'display-line-numbers)
(setq-default display-line-numbers-width 3)
(global-display-line-numbers-mode)


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


;; highlight-parentheses
(require 'highlight-parentheses)
(global-highlight-parentheses-mode)


;; savehist
(require 'savehist)
(setq savehist-additional-variables '(extended-command-history))
(savehist-mode 1)


(provide 'init)
;;; init.el ends here
