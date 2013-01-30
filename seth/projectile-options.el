(eval-after-load "grep"
  '(progn
     (add-to-list 'grep-find-ignored-directories ".lein*")
     (add-to-list 'grep-find-ignored-directories "resources/public/js/cljs")
     (add-to-list 'grep-find-ignored-directories "migrations")
     (add-to-list 'grep-find-ignored-directories "target")))
