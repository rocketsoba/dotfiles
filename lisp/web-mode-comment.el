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
