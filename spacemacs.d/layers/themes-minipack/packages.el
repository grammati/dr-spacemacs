;; packages.el --- Themes Mini-Pack Layer packages File for Spacemacs
;;
;; Copyright (c) 2014-2015 Chris Perkins
;;
;; Author: Chris Perkins <chrisperkins99@gmail.com>
;; URL: https://github.com/RallySoftware/dr-spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq themes-minipack-packages
  '(
    busybee-theme
    color-theme-sanityinc-tomorrow
    cyberpunk-theme
    dakrone-theme
    darkburn-theme
    grandshell-theme
    gruber-darker-theme
    jazz-theme
    lush-theme
    mustang-theme
    niflheim-theme
    noctilux-theme
    seti-theme
    subatomic256-theme
    tango-2-theme
    tangotango-theme
    twilight-theme
    ujelly-theme
    zen-and-art-theme
    zenburn-theme
    ))

;; programmatically defin the init functions
(dolist (pkg themes-minipack-packages)
  (eval `(defun ,(intern (format "themes-minipack/init-%S" pkg)) nil)))
