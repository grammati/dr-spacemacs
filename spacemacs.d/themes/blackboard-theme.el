;; Blackboard Colour Theme for Emacs.
;;
;; Defines a colour scheme resembling that of the original TextMate Blackboard colour theme.
;; To use add the following to your .emacs file (requires the color-theme package):
;;
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "~/.emacs.d/themes/color-theme-blackboard.el")
;;
;; And then (color-theme-blackboard) to activate it.
;;
;; MIT License Copyright (c) 2008 JD Huntington <jdhuntington at gmail dot com>
;; Credits due to the excellent TextMate Blackboard theme
;;
;; All patches welcome

(deftheme blackboard "A terminal-friendly variation on the Blackboard theme")

(custom-theme-set-faces
 'blackboard
 '(default                              ((t (:background "#121212" :foreground "#F8F8F8"))))
 '(cursor                               ((t (:foreground "#A7A7A7"))))
 '(region                               ((t (:background "#763B25"))))
 '(blue                                 ((t (:foreground "blue"))))
 '(bold                                 ((t (:bold t))))
 '(bold-italic                          ((t (:bold t))))
 '(border-glyph                         ((t (nil))))
 '(buffers-tab                          ((t (:background "#121212" :foreground "#F8F8F8"))))
 '(font-lock-builtin-face               ((t (:foreground "#94bff3"))))
 '(font-lock-comment-face               ((t (:background "#121212" :foreground "#AEAEAE"))))
 '(font-lock-constant-face              ((t (:foreground "#D8FA3C"))))
 '(font-lock-doc-string-face            ((t (:foreground "DarkOrange"))))
 '(font-lock-function-name-face         ((t (:foreground "#FF6400"))))
 '(font-lock-keyword-face               ((t (:foreground "#FBDE2D"))))
 '(font-lock-preprocessor-face          ((t (:foreground "Aquamarine"))))
 '(font-lock-reference-face             ((t (:foreground "SlateBlue"))))
 '(font-lock-regexp-grouping-backslash  ((t (:foreground "#E9C062"))))
 '(font-lock-regexp-grouping-construct  ((t (:foreground "red"))))

 '(font-lock-string-face                ((t (:foreground "#61CE3C"))))
 '(font-lock-type-face                  ((t (:foreground "#8DA6CE"))))
 '(font-lock-variable-name-face         ((t (:foreground "#FF6400"))))
 '(font-lock-warning-face               ((t (:bold t :foreground "Pink"))))
 '(gui-element                          ((t (:background "#D4D0C8" :foreground "black"))))
 '(mode-line                            ((t (:background "grey75" :foreground "black"))))
 '(highlight                            ((t (:background "#222222"))))
 '(highline-face                        ((t (:background "SeaGreen"))))
 '(italic                               ((t (nil))))
 '(left-margin                          ((t (nil))))
 '(text-cursor                          ((t (:background "yellow" :foreground "black"))))
 '(toolbar                              ((t (nil))))
 '(underline                            ((nil (:underline nil))))
 '(zmacs-region                         ((t (:background "snow" :foreground "ble"))))

 '(minibuffer-prompt                    ((t (:bold t :foreground "#FF9999"))))
 '(idle-highlight                       ((t (:background "#482822"))))
 '(tide-hl-identifier-face              ((t (:background "#482822"))))
 '(isearch-fail                         ((t (:background "red4"))))
 '(clojure-test-error-face              ((t (:background "orange3"))))
 '(magit-item-highlight                 ((t (:background nil))))
 '(hl-line                              ((t (:background "#363636"))))

 ;; Helm
 '(helm-header                          ((t (:background "#242424"))))

 ;; To make powerline pretty
 '(mode-line                            ((t (:background "gray10" :foreground "#4c83ff"))))
 '(mode-line-inactive                   ((t (:background "gray10" :foreground "gray30"))))

 '(ediff-even-diff-A                    ((t (:background "#484848" :foreground "Grey"))))
 '(ediff-even-diff-B                    ((t (:background "#484848" :foreground "Grey"))))
 '(ediff-even-diff-C                    ((t (:background "#484848" :foreground "Grey"))))
 '(ediff-even-diff-Ancestor             ((t (:background "#484848" :foreground "Grey"))))
 '(ediff-odd-diff-A                     ((t (:background "#484848" :foreground "Grey"))))
 '(ediff-odd-diff-B                     ((t (:background "#484848" :foreground "Grey"))))
 '(ediff-odd-diff-C                     ((t (:background "#484848" :foreground "Grey"))))
 '(ediff-odd-diff-Ancestor              ((t (:background "#484848" :foreground "Grey"))))

 '(cider-debug-code-overlay-face        ((t (:background "#color-52"))))
 '(cider-result-overlay-face            ((t (:background "color-58"))))

 '(evil-search-highlight-persist-highlight-face ((t (:background "#281812"))))
 ;; '(lazy-highlight ((t (:background "#182812"))))
 ;; '(isearch ((t (::foreground "#3E3D31" :background "DarkGoldenrod"))))

 )


(provide-theme 'blackboard)
