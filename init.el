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
                                  ir-black-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; add themes folder to theme load path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(load-theme 'ir-black t)

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
  (setq mouse-sel-mode t)

  ;; Enable copy and paste with Mac
  ;; requires https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;; Window system specific configurations
(when window-system
  (set-face-attribute 'default nil :font "Droid Sans Mono" :height 150) ;; font & font size
  (server-start)
  (eshell))

;; Globals
(global-rainbow-delimiters-mode)
(projectile-global-mode)
(setq projectile-globally-ignored-files '("TAGS" "*.js"))

;; Bindings
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-x N") 'nrepl-jack-in)
(global-set-key (kbd "C-x B") 'projectile-find-file)
(global-set-key (kbd "C-x T") 'ns-toggle-fullscreen)

;; Paredit in the terminal
(global-set-key (kbd "C-c 0") 'paredit-forward-slurp-sexp)
(global-set-key (kbd "C-c 9") 'paredit-backward-slurp-sexp)
(global-set-key (kbd "C-c ]") 'paredit-forward-barf-sexp)
(global-set-key (kbd "C-c [") 'paredit-backward-barf-sexp)

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; clojure-mode
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(setq clojure-mode-inf-lisp-command "lein trampoline cljsbuild repl-listen")

;; cosmetics
;; lose the stupid pipe chars on the split-screen bar
(set-face-foreground 'vertical-border "#202020")
(set-face-background 'vertical-border "#202020")

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

(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

;; remove fancy f for anonymous functions
(remove-hook 'clojure-mode-hook 'esk-pretty-fn)

;; If running Mac OS X set up dash functionality
(when (not (eq (display-graphic-p) 'x))
  ;; Dash
  (defun open-in-dash (docset query)
    (interactive "sDocset: \nsQuery: \n")
    (shell-command (concat "open" " dash://" docset ":" query)))

  (defun open-word-in-dash (docset)
    (interactive "sDocset: \n")
    (shell-command (concat "open" " dash://" docset ":" (current-word))))

  (global-set-key (kbd "C-c d") 'open-in-dash)
  (global-set-key (kbd "C-c D") 'open-word-in-dash))

;; Project notes
(defun save-note (note)
  (interactive "sNote: \n")
  (append-to-file (format "* %s\n" note) nil "~/Dropbox/Notes/ProjectNotes.md"))

(global-set-key (kbd "C-c n") 'save-note)

(defun open-notes ()
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer (find-file-noselect "~/Dropbox/Notes/ProjectNotes.md" t))
  (revert-buffer t t t))

(global-set-key (kbd "C-c N") 'open-notes)

