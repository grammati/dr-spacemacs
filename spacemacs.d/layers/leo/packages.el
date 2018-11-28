(defconst leo-packages
  '(
    exec-path-from-shell

    ;; Better lisp state
    evil-lisp-state

    ;; evil-smartparents makes it harder to unbalance parens in evil's normal state
    evil-smartparens


    ;; the built-in auto-highlight-symbol is obnoxious - idle-highlight is much nicer
    idle-highlight-mode

    ;; IDE for javascript
    indium

    ;; Making multiple cursors work well with evil is an ongoing struggle
    multiple-cursors

    ;; paredit is still better than smartparens
    paredit

    ;; auto-save buffers when they lose focus
    super-save

    typescript-mode
    prettier-js
    flycheck-jest

    ;; These packages should already be included, either because they are part
    ;; of spacemacs, or because they are required by a layer that we declare in
    ;; leo/layers.el, but we include them here so that we can have post-init
    ;; functions to configure them.
    cider
    magit
    neotree
    open-junk-file
    pbcopy
    popwin
    projectile
    ))

(defun leo/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :config (exec-path-from-shell-initialize)))

(defconst leo/evil-lisp-state-overrides
  `(("$" . end-of-defun)
    ("A" . beginning-of-defun)
    ("N" . paredit-forward-down)
    ("dd" . sp-kill-sexp)
    ("n" . paredit-forward-up)))

(defun leo-make-evil-lisp-state-better ()
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
    :config (leo-make-evil-lisp-state-better)))

(defun leo/init-prettier-js ()
  (use-package prettier-js
    :init (progn
            (add-hook 'js2-mode-hook 'prettier-js-mode)
            (add-hook 'json-mode-hook 'prettier-js-mode)
            (add-hook 'typescript-mode 'prettier-js-mode))))

(defun leo/post-init-typescript-mode ()
  (use-package typescript-mode
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'typescript-mode
        "="  'prettier-js))))

(defun leo/post-init-open-junk-file ()
  (use-package open-junk-file
    :config (progn
              (setq open-junk-file-format (concat (expand-file-name user-emacs-directory)
                                                  ".cache/junk/%Y/%m/%d.")))))
(defun leo/post-init-indium ()
  (use-package indium
    :defer t
    :init (progn
            (message "Indium loaded"))))

(defun leo/post-init-paredit ()
  (use-package paredit))

(defun leo/post-init-projectile ()
  (use-package projectile
    :config (progn
              ;;(leo-projectile-configure-for-amazon)
              (advice-add #'projectile-invalidate-cache :before (lambda (&rest _) (recentf-cleanup))))))

(defun leo/init-pbcopy ()
  (use-package pbcopy
    :init (turn-on-pbcopy)))

(defun leo/post-init-magit ()
  (use-package magit
    :defer t
    :init (progn
            (setq magit-display-buffer-function #'leo-magit-display-buffer-function))))

(defun leo/init-evil-smartparens ()
  (use-package evil-smartparens
    :defer t))

(defun turn-on-idle-highlight-mode ()
  (idle-highlight-mode 1))

(defun leo/init-idle-highlight-mode ()
  (use-package idle-highlight-mode
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'turn-on-idle-highlight-mode))))

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
      ;;       super-save-idle-duration 3)
      (dolist (f '(select-window
                   select-window-by-number
                   ace-select-window))
        (add-to-list 'super-save-triggers f))
      (super-save-initialize)
      (add-hook 'evil-insert-state-exit-hook 'super-save-command t)
      (leo-stfu 'super-save-command))))

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

      ;; Clojurescript
      ;; (set 'cider-cljs-lein-repl "(do (user/fig-start) (user/cljs-repl))")
      (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

      ;; Why on earth is this not the default?
      (setq cider-repl-use-pretty-printing t)

      (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
        "sc" 'leo-cider-find-and-clear-repl-buffer
        "td" 'leo-cider-debug-test-at-point)
      (leo-cider-configure-debugging)

      ;; Override backtick binding from smartparens
      (with-eval-after-load 'smartparens
        (sp-local-pair sp-lisp-modes "`" nil :actions nil)))))

(defun leo/post-init-popwin ()
  ;; popwin is annoying, but trying to exclude the package from spacemacs makes
  ;; stuff blow up, so just configure it to never do anything
  (setq popwin:special-display-config nil))

(defun leo/post-init-neotree ()
  (spacemacs/set-leader-keys "ot" 'leo-neotree-find-buffer-file))
