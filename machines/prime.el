
(setenv "PATH" (concat "/Users/cygnus/.bin:/Users/cygnus/.cabal/bin:/opt/local/bin:"
                       (getenv "PATH")))

(if window-system
    (progn
      (set-default-font "Menlo:size=15")
      )
)

(load-theme 'jtd-tsdh-dark)

;; (color-theme-dark-blue2)

;; (add-load-path "emacs_work")
;; (load-library "dotemacs")

;; (set-variable 'skeleton-pair t)
;; (global-set-key "(" 'skeleton-pair-insert-maybe)
;; (global-set-key "[" 'skeleton-pair-insert-maybe)
;; (global-set-key "{" 'skeleton-pair-insert-maybe)
;; (global-set-key "\"" 'skeleton-pair-insert-maybe)
