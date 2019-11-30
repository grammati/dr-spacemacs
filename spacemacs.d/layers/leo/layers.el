(configuration-layer/declare-layers
 '(
   auto-completion
   (clojure :variables
            ;; clojure-enable-sayid t
            clojure-enable-clj-refactor t
            clojure-enable-linters 'clj-kondo)
   csv
   emacs-lisp
   git
   github
   helm
   html
   javascript
   markdown
   multiple-cursors
   neotree
   osx
   prettier
   (ranger :variables
           ranger-enter-with-minus t
           ranger-max-preview-size 10 ; MB
           ranger-show-hidden t
           ranger-show-literal nil)
   restclient
   (shell-scripts :variables shell-scripts-backend 'lsp)
   spell-checking
   (syntax-checking :variables syntax-checking-enable-tooltips nil)
   themes-minipack
   (typescript :variables
               typescript-backend 'tide
               typescript-fmt-on-save nil
               typescript-fmt-tool 'prettier)
   yaml
   ))


