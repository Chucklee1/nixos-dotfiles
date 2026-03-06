;;; -*- exwm.el -*- no-byte-compile: t; -*
(require 'exwm)
(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

;; Set the default number of workspaces
(setq exwm-workspace-number 5)

;; When window "class" updates, use it to set the buffer name
;; (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

;; These keys should always pass through to Emacs
(setq exwm-input-prefix-keys
      '(?\C-x
        ?\C-u
        ?\C-h
        ?\M-x
        ?\M-`
        ?\M-&
        ?\M-:
        ?\C-\M-j  ;; Buffer list
        ?\C-\ ))  ;; Ctrl+Space

;; Ctrl+Q will enable the next key to be sent directly
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;; Set up global key bindings.  These always work, no matter the input state!
;; Keep in mind that changing this list after EXWM initializes has no effect.
(setq exwm-input-global-keys
      `(
        ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
        ([?\s-r] . exwm-reset)

        ;; Move between windows
        ([s-left] . windmove-left)
        ([s-right] . windmove-right)
        ([s-up] . windmove-up)
        ([s-down] . windmove-down)

        ;; Launch applications via shell command
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))

        ;; Switch workspace
        ([?\s-w] . exwm-workspace-switch)

        ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 1 5))))

(exwm-wm-mode)
