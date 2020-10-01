(configuration-layer/declare-layers
 '(
   auto-completion
   (clojure :variables
            clojure-enable-sayid t
            clojure-enable-clj-refactor t
            clojure-enable-linters 'clj-kondo)
   csv
   emacs-lisp
   git
   github
   helm
   html
   java
   (javascript :variables
               js-indent-level 2)
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
   (react :variables
          web-mode-code-indent-offset 2)
   (restclient :variables restclient-inhibit-cookies t)
   (shell-scripts :variables shell-scripts-backend 'lsp)
   spell-checking
   (syntax-checking :variables syntax-checking-enable-tooltips nil)
   themes-minipack
   (typescript :variables
               typescript-indent-level 2
               typescript-linter 'eslint
               typescript-backend 'tide
               typescript-fmt-on-save nil
               typescript-fmt-tool 'prettier)
   yaml
   ))


