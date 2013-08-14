;; If running Mac OS X set up dash functionality
(when (not (eq (display-graphic-p) 'x))
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
  (setq interprogram-paste-function 'copy-from-osx)
  
  ;; Dash
  (defun open-in-dash (docset query)
    (interactive "sDocset: \nsQuery: \n")
    (shell-command (concat "open" " dash://" docset ":" query)))

  (defun open-word-in-dash (docset)
    (interactive "sDocset: \n")
    (shell-command (concat "open" " dash://" docset ":" (current-word)))))

;; Project notes
(defun save-note (note)
  (interactive "sNote: \n")
  (append-to-file (format "* %s\n" note) nil "~/Dropbox/Notes/ProjectNotes.md"))

(defun open-notes ()
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer (find-file-noselect "~/Dropbox/Notes/ProjectNotes.md" t))
  (revert-buffer t t t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Taken from Nicolas Lara (https://github.com/nicolaslara/emacs/blob/master/functions.el)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; kill-other-buffers
;; ------------------
;; I find that Emacs buffers multiply faster than rabbits.  They were
;; regenerating faster than I could kill them so I wrote this.
;; (Original
;; version was my first code in ELisp!)  Run this macro to kill all
;; but the
;; active buffer and the unsplit the window if need be.
;;
(defun kill-other-buffers ()
  "Kill all buffers except the current and unsplit the window."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))   ; Delete other buffers
  (delete-other-windows)                                      ; And then unsplit the current window...
  (delete-other-frames))                                      ; ...and remove other frames, too.

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ac-nrepl tab completion (https://github.com/clojure-emacs/ac-nrepl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
