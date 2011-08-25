;;; parenthesis.el --- Insert pair of parenthesis

;; Copyright (C) 2007  khiker

;; Author: khiker <khiker.mail+elisp@gmail.com>
;; Keywords: lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Insert Pair of parenthesis.
;; if region is active, insert enclosing strings at region boundaries.

;; Functions:

;; style of {}   : insert-braces
;; style of []   : insert-brackets
;; style of ()   : insert-parens
;; style of ''   : insert-single-quotation
;; style of ""   : insert-double-quotation
;; style of <>   : insert-angle

;; style of `'   : insert-grave-and-quotation

;; style of {  } : insert-braces2
;; style of [  ] : insert-brackets2
;; style of (  ) : insert-parens2
;; style of '  ' : insert-single-quotation2
;; style of "  " : insert-double-quotation2
;; style of <  > : insert-angle2

;; Prefix Arg Example:

;; C- - C-4 insert-parens => ((((
;; C-0      insert-parens => (
;; C-4      insert-parens => (((())))

;; Note:
;; In more than Emacs 22, dotimes is defined by subr.el
;; Emacs 21 or before is defined by cl-macs.el

;;; Code:

(if (> 22 (string-to-number emacs-version))
    (eval-when-compile
      (require 'cl)))


(defconst parenthesis-version 0.1)


(defun insert-parenthesis-internal (arg open close &optional ignore-backslash)
  (if (or ignore-backslash
          (bobp)
          (/= (char-before) ?\\))
      (if (> arg 0)
          (progn
            (save-excursion
              (save-restriction
                (if mark-active
                    (narrow-to-region (region-beginning) (region-end))
                  (narrow-to-region (point) (point)))
                (goto-char (point-min))
                (dotimes (i arg) (insert open))
                (goto-char (point-max))
                (dotimes (i arg) (insert close))))
            (forward-char (* (length open) arg)))
        (dotimes (i (if (= arg 0) 1 (- arg))) (insert open)))
    (self-insert-command arg)))


(defun parenthesis-register-keys (str map)
  "register keys to local-map."
  (let ((char-list '(?\" ?' ?\( ?{ ?\[ ?< ?`))
        len ch)
    (setq len (length str))
    (dotimes (i len)
      (setq ch (aref str i))
      (message "%s" ch)
      (cond
       ((= ch ? )
        (setq i (1+ i))
        (when (and (> len i)
                   (setq ch (aref str i))
                   (parenthesis-search-function ch))
          (define-key map [ch] (parenthesis-search-function ch t))))
       ((member ch char-list)
        (define-key map (vector ch) (parenthesis-search-function ch)))
       (t
        nil)))))

(defun parenthesis-search-function (ch &optional space)
  (cond
   ((= ch ?{)
    (intern (concat "insert-braces" (if space "2"))))
   ((= ch ?\[)
    (intern (concat "insert-brackets" (if space "2"))))
   ((= ch ?\()
    (intern (concat "insert-parens" (if space "2"))))
   ((= ch ?')
    (intern (concat "insert-single-quotation" (if space "2"))))
   ((= ch ?\")
    (intern (concat "insert-double-quotation" (if space "2"))))
   ((= ch ?<)
    (intern (concat "insert-angle" (if space "2"))))
   ((= ch ?`)
    (intern "insert-grave-and-quotation"))
   (t
    nil)))


;; style of {}
(defun insert-braces (arg)
  "A pair of brace({}) is insert automatically."
  (interactive "p")
  (insert-parenthesis-internal arg "{" "}"))

(defun insert-braces2 (arg)
  "A pair of brace({  }) is insert automatically."
  (interactive "p")
  (insert-parenthesis-internal arg "{ " " }"))


;; style of []
(defun insert-brackets (arg)
  "A pair of square bracket([]) is insert automatically."
  (interactive "p")
  (insert-parenthesis-internal arg "[" "]"))

(defun insert-brackets2 (arg)
  "A pair of square bracket([  ]) is insert automatically."
  (interactive "p")
  (insert-parenthesis-internal arg "[ " " ]"))


;; style of ()
(defun insert-parens (arg)
  "A pair of round bracket\"()\" is insert automatically."
  (interactive "p")
  (insert-parenthesis-internal arg "(" ")"))

(defun insert-parens2 (arg)
  "A pair of round bracket\"(  )\" is insert automatically."
  (interactive "p")
  (insert-parenthesis-internal arg "( " " )"))


;; style of ''
(defun insert-single-quotation (arg)
  "A pair of single quotation('') is insert automatically."
  (interactive "p")
  (insert-parenthesis-internal arg "'" "'"))

(defun insert-single-quotation2 (arg)
  "A pair of single quotation('  ') is insert automatically."
  (interactive "p")
  (insert-parenthesis-internal arg "' " " '"))


;; style of ""
(defun insert-double-quotation (arg)
  "A pair of double quotation(\"\") is insert automatically."
  (interactive "p")
  (insert-parenthesis-internal arg "\"" "\""))

(defun insert-double-quotation2 (arg)
  "A pair of double quotation(\"  \") is insert automatically."
  (interactive "p")
  (insert-parenthesis-internal arg "\" " " \""))

;; style of <>
(defun insert-angle (arg)
  "A pair of angle(<>) is insert automatically."
  (interactive "p")
  (insert-parenthesis-internal arg "<" ">"))

(defun insert-angle2 (arg)
  "A pair of angle(<  >) is insert automatically."
  (interactive "p")
  (insert-parenthesis-internal arg "< " " >"))


;; style of `'
(defun insert-grave-and-quotation (arg)
  "insert greve and quatation(`')."
  (interactive "p")
  (insert-parenthesis-internal arg "`" "'"))

(provide 'parenthesis)

;;; parenthesis.el ends here
