;; lose the stupid pipe chars on the split-screen bar
(set-face-foreground 'vertical-border "white")
(set-face-background 'vertical-border "white")

(setq custom-safe-themes
      '("b7553781f4a831d5af6545f7a5967eb002c8daeee688c5cbf33bf27936ec18b3" default))

(defun zb ()
  (interactive)
  (load-theme 'zenburn)
  (set-face-background 'vertical-border "black")
  (set-face-foreground 'vertical-border "black")
  (require 'hl-line)
  (set-face-background 'hl-line "gray17")
  (eval-after-load 'magit
    '(set-face-background 'magit-item-highlight "black"))
  (set-face-foreground 'eshell-prompt "turquoise"))

(defun wombat ()
  (interactive)
  (load-theme 'wombat)
  (set-face-background 'vertical-border "black")
  (set-face-foreground 'vertical-border "black")
  (require 'hl-line)
  (set-face-background 'hl-line "gray17")
  (eval-after-load 'magit
    '(set-face-background 'magit-item-highlight "black")))

;; font size
(set-face-attribute 'default nil :height 150)

;; screen size
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 184))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))

(set-frame-size-according-to-resolution)

(wombat)
