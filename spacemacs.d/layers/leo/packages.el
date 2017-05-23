(defconst leo-packages
  '(
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

    ;; These packages should already be included, either because they are part
    ;; of spacemacs, or because they are required by a layer that we declare in
    ;; leo/layers.el, but we include them here so that we can have post-init
    ;; functions to configure them.
    cider
    magit
    neotree
    pbcopy
    popwin
    projectile
    ))

(defun leo/post-init-indium ()
  (use-package indium
    :defer t
    :init (progn
            (message "Indium loaded"))))

(defun leo/post-init-paredit ()
  (use-package paredit))

(defun leo/post-init-projectile ()
  (advice-add #'projectile-invalidate-cache :before (lambda (&rest _) (recentf-cleanup))))

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
      (setq super-save-auto-save-when-idle nil
            super-save-idle-duration 3)
      (dolist (f '(select-window
                   select-window-by-number
                   ace-select-window))
        (add-to-list 'super-save-triggers (symbol-name f)))
      (super-save-initialize))))

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
      ;; Set up repl to show, but not focus
      (setq cider-repl-pop-to-buffer-on-connect nil)
      (advice-add #'cider-repl-init :after #'display-buffer)

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

      ;; Override backtick binding from smartparens
      (with-eval-after-load 'smartparens
          (sp-local-pair sp-lisp-modes "`" nil :actions nil)))))

(defun leo/post-init-popwin ()
  ;; popwin is annoying, but trying to exclude the package from spacemacs makes
  ;; stuff blow up, so just configure it to never do anything
  (setq popwin:special-display-config nil))

(defun leo/post-init-neotree ()
  (spacemacs/set-leader-keys "ot" 'leo-neotree-find-buffer-file))
