;;; Config


;; Potentially controversial settings: make things a bit less vimmy
(setq
 evil-cross-lines      t
 evil-move-beyond-eol  t
 evil-move-cursor-back nil
 evil-visual-char      'exclusive

 ;; I'm not sure why this is even an option - undo is unusable if this is
 ;; not set to t
 evil-want-fine-undo   t

 js2-basic-offset      2
 web-mode-code-indent-offset 2)
