;; make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
;; increase the amount of data which emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(defconst CONFIG_PATH (expand-file-name "~/.emacs.d/init.el"))
(defconst ORGCFG_PATH (expand-file-name "~/.emacs.d/init.org"))

(let ((my/path (expand-file-name "~/.nix-profile/bin")))
  (setenv "PATH" (concat my/path ":" (getenv "PATH")))
  (add-to-list 'exec-path my/path))
