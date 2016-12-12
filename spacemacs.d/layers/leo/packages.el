(defconst leo-packages
  '(
    ;; evil-smartparents makes it harder to unbalance parens in evil's normal state
    evil-smartparens

    ;; the built-in auto-highlight-symbol is obnoxious - idle-highlight is much nicer
    idle-highlight-mode

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
    neotree
    popwin
    restclient
    ))


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
  (require 'super-save)
  (dolist (f '(select-window
               select-window-by-number
               ace-select-window))
    (add-to-list 'super-save-triggers (symbol-name f)))
  (super-save-initialize))

(defun leo/post-init-cider ()
  (use-package cider
    :defer t
    :init
    (progn
      (dolist (hook '(clojure-mode-hook cider-repl-mode-hook lisp-mode-hook emacs-lisp-mode-hook))
        (add-hook hook
                  (lambda ()
                    ;; TODO - figure out how to enable paredit in insert mode only,
                    ;; and evil-smartparens in normal node only.
                    (paredit-mode)
                    (evil-smartparens-mode)))))
    :config
    (progn
      ;; Set up repl to show, but not focus
      (setq cider-repl-pop-to-buffer-on-connect nil)
      (advice-add #'cider-repl-init :after #'display-buffer)

      ;; Configure clj-refactor
      (setq cljr-favor-prefix-notation nil)

      ;; Clojurescript
      (set 'cider-cljs-lein-repl "(do (user/fig-start) (user/cljs-repl))")

      (spacemacs/set-leader-keys-for-major-mode 'clojure-mode
        "sc" 'leo-cider-find-and-clear-repl-buffer))))

(defun leo/post-init-popwin ()
  ;; popwin is annoying, but trying to exclude the package from spacemacs makes
  ;; stuff blow up, so just configure it to never do anything
  (setq popwin:special-display-config nil))

(defun leo/post-init-neotree ()
  (spacemacs/set-leader-keys "ot" 'leo-neotree-find-buffer-file))

(defun leo/post-init-restlclient ()
  (spacemacs/set-leader-keys
    "ji" 'helm-imenu))
