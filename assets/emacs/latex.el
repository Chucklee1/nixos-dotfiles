(use-package auctex
  :ensure t
  :defer t
  )

(setq TeX-view-program-list
      '(("Zathura" "zathura %o")))

(setq TeX-view-program-selection
      '((output-pdf "Zathura")
        (output-dvi "xdvi")
        (output-html "xdg-open")))
(setq TeX-engine 'luatex)
