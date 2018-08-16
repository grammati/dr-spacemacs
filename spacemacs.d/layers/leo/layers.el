(configuration-layer/declare-layers
 '(
   ;; General / Essential
   auto-completion
   git
   github
   helm
   (syntax-checking :variables syntax-checking-enable-tooltips nil)

   ;; Languages / Major-modes
   clojure
   csv
   emacs-lisp
   ess                                  ; R
   (html :variables web-beautify-args '("--editorconfig" "-s" "2" "-f" "-"))
   (java :variables java-backend nil)
   javascript
   markdown
   nginx
   python ipython-notebook
   react
   ruby
   (scala :variables scala-enable-eldoc t)
   sql
   (typescript :variables
               typescript-fmt-on-save nil)
   yaml

   ;; Tools and stuff
   command-log
   docker
   prodigy
   restclient
   spell-checking

   ;; Color themes
   themes-minipack
   ))
