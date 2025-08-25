(use-package auctex
  :ensure t
  :defer t
  :hook (LaTeX-mode . (lambda ()
			(push (list 'output-pdf "Zathura")
			      TeX-view-program-selection))))

(setq TeX-engine 'luatex)
