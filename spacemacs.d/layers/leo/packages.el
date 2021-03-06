(defconst leo-packages
  '(
    cider
    evil-lisp-state
    evil-smartparens
    evil-terminal-cursor-changer
    exec-path-from-shell
    graphql-mode
    idle-highlight-mode ;; the built-in auto-highlight-symbol is obnoxious - idle-highlight is much nicer
    jest
    magit
    neotree
    open-junk-file
    paredit ;; paredit is still better than smartparens
    popwin
    projectile
    ;; python
    super-save ;; auto-save buffers when they lose focus
    typescript-mode
    ))

(defun leo/init-jest ()
  (use-package jest
    :config (progn
              (setq jest-executable "npx jest -i"))))

(defun leo/init-graphql-mode ()
  (use-package graphql-mode))

(defun leo/init-evil-terminal-cursor-changer ()
  (use-package evil-terminal-cursor-changer
    :init (unless (display-graphic-p)
            (evil-terminal-cursor-changer-activate))))

(defun leo/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :config (exec-path-from-shell-initialize)))

(defconst leo/evil-lisp-state-overrides
  `(("$" . end-of-defun)
    ("A" . beginning-of-defun)
    ("N" . paredit-forward-down)
    ("dd" . sp-kill-sexp)
    ("n" . paredit-forward-up)))

(defun leo/make-evil-lisp-state-better ()
  (dolist (x leo/evil-lisp-state-overrides)
    (let ((key (car x))
          (cmd (cdr x)))
      (eval
       `(progn
          (if evil-lisp-state-global
              (define-key evil-lisp-state-map ,(kbd key)
                (evil-lisp-state-enter-command ,cmd))
            (define-key evil-lisp-state-major-mode-map ,(kbd key)
              (evil-lisp-state-enter-command ,cmd))))))))

(defun leo/post-init-evil-lisp-state ()
  (use-package evil-lisp-state
    :config (leo/make-evil-lisp-state-better)))

(defun leo/post-init-typescript-mode ()
  (use-package typescript-mode
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
        "f"  'prettier-js
        "e e" 'leo/execute-ts-file
        "e E" 'leo/execute-ts-file-with-args))))

(defun leo/post-init-open-junk-file ()
  (use-package open-junk-file
    :config (progn
              (setq open-junk-file-format (concat (expand-file-name user-emacs-directory)
                                                  ".cache/junk/%Y/%m/%d.")))))

(defun leo/post-init-paredit ()
  (use-package paredit))

(defun leo/post-init-projectile ()
  (use-package projectile
    :config (progn
              ;; After projectile-invalidate-cache, you still see deleted files in helm. This fixes it.
              (advice-add #'projectile-invalidate-cache :before (lambda (&rest _) (recentf-cleanup))))))

(defun leo/post-init-magit ()
  (use-package magit
    :defer t
    :config (progn
              ;; This doesn't work. Seriously, what the fuck, why does nothing ever fucking work in emacs? Everything is fucked.
              (setq evil-magit-want-horizontal-movement t))
    :init (progn
            ;; (setq magit-display-buffer-function #'leo/magit-display-buffer-function)
            )))

(defun leo/init-evil-smartparens ()
  (use-package evil-smartparens
    :defer t))

(defun turn-on-idle-highlight-mode ()
  (idle-highlight-mode 1))

(defun turn-off-idle-highlight-mode ()
  (idle-highlight-mode 0))

(defun leo/init-idle-highlight-mode ()
  (use-package idle-highlight-mode
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'turn-on-idle-highlight-mode)
      ;; turn of for typescript - tide does it better
      ;;(add-hook 'typescript-mode-hook 'turn-off-idle-highlight-mode)
      )))

(defun leo/init-multiple-cursors ()
  (use-package multiple-cursors
    :defer t
    :init
    (progn
      (global-set-key (kbd "C-x M-'") 'mc/edit-lines)
      (global-set-key (kbd "M-+")     'mc/mark-next-like-this)
      (global-set-key (kbd "M-_")     'mc/skip-to-next-like-this)
      (global-set-key (kbd "C-x M-+") 'mc/mark-all-like-this)
      (global-set-key (kbd "C-x _")   'mc-hide-unmatched-lines-mode)
      )))

(defun leo/init-super-save ()
  (use-package super-save
    :init
    (progn
      (require 'super-save)
      ;; (setq super-save-auto-save-when-idle t
      ;;       super-save-idle-duration 2)
      (dolist (f '(select-window
                   select-window-by-number
                   ace-select-window))
        (add-to-list 'super-save-triggers f))
      (super-save-initialize)
      (add-hook 'evil-insert-state-exit-hook 'super-save-command t)
      (leo/stfu 'super-save-command))))

(defun leo/post-init-cider ()
  (use-package cider
    :defer t
    :init
    (progn
      (dolist (hook '(clojure-mode-hook cider-repl-mode-hook lisp-mode-hook emacs-lisp-mode-hook))
        (add-hook hook
                  (lambda ()
                    (paredit-mode)
                    (smartparens-strict-mode)
                    (evil-smartparens-mode)))))
    :config
    (progn
      (setq cider-clojure-cli-global-options "-A:dev")
      (setq cider-shadow-cljs-global-options "-A:dev")

      ;; Don't show help banner in repl
      (setq cider-repl-display-help-banner nil)

      ;; Set up repl to show, but not focus
      (setq cider-repl-pop-to-buffer-on-connect nil)
      (advice-add #'cider-repl-init :after #'display-buffer)

      ;; Sweet keybindings
      (evil-define-key 'normal cider-repl-mode-map "(" 'leo-cider-open-round)

      ;; Make is so jump-to-definition does not put point on the very bottom line
      (advice-add 'cider-jump-to :after (lambda (&rest _) (recenter-top-bottom)))

      ;; Configure clj-refactor
      (setq cljr-favor-prefix-notation nil)

      ;; Why on earth is this not the default?
      (setq cider-repl-use-pretty-printing t)

      (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
        "sc" 'leo/cider-find-and-clear-repl-buffer
        "td" 'leo/cider-debug-test-at-point)

      (spacemacs/set-leader-keys-for-major-mode 'clojurescript-mode
        "tn" 'leo/cider-run-tests)

      (leo/cider-configure-debugging)

      ;; Override backtick binding from smartparens
      (with-eval-after-load 'smartparens
        (sp-local-pair sp-lisp-modes "`" nil :actions nil))

      (setq clojure-align-binding-forms
            (append clojure-align-binding-forms
                    '("p/let" "promesa.core/let")))

      )))

(defun leo/post-init-popwin ()
  ;; popwin is annoying, but trying to exclude the package from spacemacs makes
  ;; stuff blow up, so just configure it to never do anything
  (setq popwin:special-display-config nil))

(defun leo/post-init-neotree ()
  (use-package neotree
    :init (spacemacs/set-leader-keys "ot" 'leo/neotree-find-buffer-file)))
