(defun ac-php-candidate-ac ()
  (setq ac-php-prefix-str ac-prefix)
  (if (string-match "if" ac-php-prefix-str)
      nil
    (ac-php-candidate)
    )
  )
