
(setenv "PATH" (concat "/Users/cygnus/.bin:/Users/cygnus/.cabal/bin:/opt/local/bin:"
                       (getenv "PATH")))

(set-default-font "Menlo:size=15")
;; (set-default-font "Lucida Console:size=16")

(if window-system
    (progn
      (tool-bar-mode nil)
      (menu-bar-mode nil)
      )
)
;; (my-color-theme-light)
(my-color-theme-dark)

;;(add-load-path "emacs_work")
;;(load-library "dotemacs")

;; (set-variable 'skeleton-pair t)
;; (global-set-key "(" 'skeleton-pair-insert-maybe)
;; (global-set-key "[" 'skeleton-pair-insert-maybe)
;; (global-set-key "{" 'skeleton-pair-insert-maybe)
;; (global-set-key "\"" 'skeleton-pair-insert-maybe)
