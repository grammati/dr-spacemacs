;;; Config

(setq
 ;; So h/l will move to prev/next line
 evil-cross-lines      t

 ;; Without this, it's really hard to eval an expr with `, e e`
 evil-move-beyond-eol  t

 ;; I'm not sure why this is even an option - undo is unusable if this is
 ;; not set to t
 evil-want-fine-undo   t

 ;; window splitting
 split-height-threshold nil
 split-width-threshold  140

 ;; woohoo!
 spacemacs-useless-buffers-regexp nil

 ;; junk file
 open-junk-file-format (concat (expand-file-name user-emacs-directory)
                               ".cache/junk/%Y/%m/%d-%H.md"))
