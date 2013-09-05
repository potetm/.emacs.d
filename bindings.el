;; General
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-x N") 'nrepl-jack-in)
(global-set-key (kbd "C-x B") 'projectile-find-file)
(global-set-key (kbd "C-x T") 'ns-toggle-fullscreen)

;; Paredit in the terminal
(global-set-key (kbd "C-c 0") 'paredit-forward-slurp-sexp)
(global-set-key (kbd "C-c 9") 'paredit-backward-slurp-sexp)
(global-set-key (kbd "C-c ]") 'paredit-forward-barf-sexp)
(global-set-key (kbd "C-c [") 'paredit-backward-barf-sexp)
(global-set-key (kbd "C-M-w") 'paredit-copy-as-kill)

;; Window movement
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; If running Mac OS X set up dash functionality
(when (not (eq (display-graphic-p) 'x))
  ;; Dash
  (global-set-key (kbd "C-c d") 'open-in-dash)
  (global-set-key (kbd "C-c D") 'open-word-in-dash))

;; Project Notes
(global-set-key (kbd "C-c n") 'save-note)
(global-set-key (kbd "C-c N") 'open-notes)
