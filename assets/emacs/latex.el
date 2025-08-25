(use-package pdf-tools
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)) ;; compiles and sets up everything

(use-package auctex
  :defer t
  :after pdf-tools
  :hook ((LaTeX-mode . TeX-source-correlate-mode) ;; enable synctex
         (LaTeX-mode . TeX-PDF-mode))             ;; compile to PDF
  :config
  ;; (add-to-list 'exec-path "/Library/TeX/texbin")
  ;; (setenv "PATH" (concat "/Library/TeX/texbin:" (getenv "PATH")))
  )

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
		TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions
		  #'TeX-revert-document-buffer)

(add-hook 'pdf-view-mode-hook (lambda ()
                                (setq-local truncate-lines t)
                                (visual-line-mode -1)
                                (word-wrap nil)))
