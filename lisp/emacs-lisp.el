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
