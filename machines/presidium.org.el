
(defun my-mutt-mode ()
  (auto-fill-mode))

(setq auto-mode-alist
     (append '(("mutt\\-" . my-mutt-mode))
             auto-mode-alist))
