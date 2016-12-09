;;; Functions

(defun leo-add-checkouts ()
  "Add any directories under \"checkouts\" to the front of the
  load path. The intended usage is to symlink the
  source-directory of an emacs-lisp package so that you can hack
  on it, or use a version that is not in melpa yet."
 (let ((checkouts-dir (expand-file-name (concat user-emacs-directory "checkouts"))))
    (when (file-exists-p checkouts-dir)
      (dolist (d (directory-files-and-attributes checkouts-dir))
        (let ((dirname (car d))
              (is-dir  (car (cdr d))))
          (when (and is-dir (not (equal dirname ".")) (not (equal dirname "..")))
            (add-to-list 'load-path (expand-file-name (concat user-emacs-directory "checkouts/" dirname)))))))))

;; (require 'ansi-color)
(defun ansify-buffer ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun leo-spacemacs-user-config ()
  "Called from .spacemacs.d/init.el, after layers are configured."
  (leo-add-checkouts))

(defun leo-neotree-find-buffer-file ()
  (interactive)
  (neotree-find (buffer-file-name)))

(defun leo-cider-find-and-clear-repl-buffer ()
  (interactive)
  (cider-find-and-clear-repl-output t))
