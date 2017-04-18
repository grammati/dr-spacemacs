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
  (leo-add-checkouts)
  (setq-default
   evil-shift-width 2
   js-indent-level 2
   js2-basic-offset 2
   json-reformat:indent-width 2
   typescript-indent-level 2
   web-mode-code-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-markup-indent-offset 2
   neo-smart-open t
   ))

(defun leo-neotree-find-buffer-file ()
  (interactive)
  (neotree-find (buffer-file-name)))

(defun leo-cider-find-and-clear-repl-buffer ()
  (interactive)
  (cider-find-and-clear-repl-output t))

(defun leo-cider-debug-test-at-point ()
  (interactive)
  (cider-debug-defun-at-point)
  (cider-test-run-test))

(defun leo-magit-display-buffer-function (buffer)
  "Replacement for `magit-display-buffer-traditional', which does
  not work right."
  (display-buffer
   buffer (if (and (derived-mode-p 'magit-mode)
                   (not (memq (with-current-buffer buffer major-mode)
                              '(magit-process-mode
                                magit-revision-mode
                                magit-diff-mode
                                magit-stash-mode
                                magit-status-mode))))
              '(display-buffer-same-window)
            ;; The following is the only difference from
            ;; `magit-display-buffer-traditional' - pass t instead of nil to
            ;; display-buffer
            t)))
