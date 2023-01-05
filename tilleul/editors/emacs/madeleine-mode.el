;;; madeleine.el --- Major mode for editing Madeleine files

;;; Code:

(defconst madeleine-font-lock-keywords
  '("\\<check\\>" "\\<def\\>" "\\<fun\\>" "\\<Prop\\>" "\\<Sort\\>" "\\<Type\\>"
    ("[a-zA-Z][a-zA-Z0-9_]*" . font-lock-constant-face))
  "Font lock keywords for Madeleine.")

(define-derived-mode madeleine-mode prog-mode "Madeleine"
  "Major mode for editing Madeleine files."

  ;; Font lock support.
  (setq font-lock-defaults '(madeleine-font-lock-keywords nil nil nil nil))

  ;; Comments
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) ""))

(progn
  (add-to-list 'auto-mode-alist '("\\.mdln\\'" . madeleine-mode)))

(provide 'madeleine-mode)

;;; madeleine-mode.el ends here
