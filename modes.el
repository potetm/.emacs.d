;; all modes
(global-linum-mode t)

;; clojure-mode
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(setq clojure-mode-inf-lisp-command "lein trampoline cljsbuild repl-listen")

;; remove fancy f for anonymous functions
(remove-hook 'clojure-mode-hook 'esk-pretty-fn)

;; nrepl-mode
(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)
;; add syntax highlighting to the repl
(add-hook 'nrepl-mode-hook
      (lambda ()
        (font-lock-mode nil)
        (clojure-mode-font-lock-setup)
        (font-lock-mode t)))

;; ac-nrepl
(require 'auto-complete)
(require 'ac-nrepl)
(global-auto-complete-mode t)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

;; set TAB as auto-complete trigger
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; Use ac-nrepl docs instead of default repl docs
(define-key nrepl-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

;; clojure-script
(add-to-list 'auto-mode-alist '("\.clj$" . clojure-mode))

;; less
;; markdown
;; add file extentions to mode auto load
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; projectile
(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-directories ".lein*")
     (add-to-list 'grep-find-ignored-directories "resources")
     (add-to-list 'grep-find-ignored-directories "migrations")
     (add-to-list 'grep-find-ignored-directories "target")
     (add-to-list 'grep-find-ignored-directories "out")))
