;; set ispell program name to aspell (brew install aspell)
(setq-default ispell-program-name "aspell")

;; turn off annoying visual-bell
(setq visible-bell nil)

;; save backup files to ~/.saves
(setq backup-directory-alist `(("." . "~/.saves")))
