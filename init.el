(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; desired starter-kit packages
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-eshell
                                  starter-kit-js starter-kit-bindings
                                  clojure-mode clojure-test-mode nrepl
                                  projectile rainbow-delimiters rainbow-mode
                                  ir-black-theme less-css-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; add themes folder to theme load path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(load-theme 'ir-black t)

;; turn off annoying visual-bell
(setq visible-bell nil)

;; save backup files to ~/.saves
(setq backup-directory-alist `(("." . "~/.saves")))

;; Globals
(global-rainbow-delimiters-mode)
(projectile-global-mode)
(setq projectile-globally-ignored-files '("TAGS" "*.js"))

;; Terminal specific configurations
(unless window-system
  ;; Enable mouse support
  (require 'mouse)
  (xterm-mouse-mode t)

  (defun up-slightly () (interactive) (scroll-up 5))
  (defun down-slightly () (interactive) (scroll-down 5))

  (global-set-key (kbd "<mouse-4>") 'down-slightly)
  (global-set-key (kbd "<mouse-5>") 'up-slightly)
  (global-set-key (kbd "<menu-bar> <mouse-4>") 'down-slightly)
  (global-set-key (kbd "<menu-bar> <mouse-5>") 'up-slightly)

  (defun track-mouse (e))
  (setq mouse-sel-mode t))

;; Window system specific configurations
(when window-system
  (set-face-attribute 'default nil :font "Droid Sans Mono" :height 150) ;; font & font size
  (server-start)
  (eshell))

;; cosmetics
;; lose the stupid pipe chars on the split-screen bar
(set-face-foreground 'vertical-border "#202020")
(set-face-background 'vertical-border "#202020")
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;; other options
;; set ispell program name to aspell (brew install aspell)
(setq-default ispell-program-name "aspell")

(load "~/.emacs.d/functions")
(load "~/.emacs.d/bindings")
(load "~/.emacs.d/modes")
