(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; set theme dir and tomorrow-night theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night t)

;; desired starter-kit packages
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-eshell
                                  starter-kit-js starter-kit-bindings
                                  clojure-mode clojure-test-mode nrepl
                                  projectile rainbow-delimiters rainbow-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(projectile-global-mode)
(global-rainbow-delimiters-mode)
(setq projectile-globally-ignored-files '("TAGS" "*.js"))

;; (eshell)

;; Bindings

(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-x N") 'nrepl-jack-in)
(global-set-key (kbd "C-x B") 'projectile-find-file)
(global-set-key (kbd "C-x T") 'ns-toggle-fullscreen)

;; clojure-mode
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(setq inferior-lisp-program "lein trampoline cljsbuild repl-listen")

;; cosmetics
;; lose the stupid pipe chars on the split-screen bar
(set-face-foreground 'vertical-border "white")
(set-face-background 'vertical-border "white")

;; font size
(set-face-attribute 'default nil :height 150)

;; markdown
;; add file extentions to mode auto load
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; other options
;; set ispell program name to aspell (brew install aspell)
(setq-default ispell-program-name "aspell")

;; turn off annoying visual-bell
(setq visible-bell nil)

;; save backup files to ~/.saves
(setq backup-directory-alist `(("." . "~/.saves")))

;; projectile
(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-directories ".lein*")
     (add-to-list 'grep-find-ignored-directories "resources")
     (add-to-list 'grep-find-ignored-directories "migrations")
     (add-to-list 'grep-find-ignored-directories "target")
     (add-to-list 'grep-find-ignored-directories "out")))
