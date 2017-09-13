(configuration-layer/declare-layers
 '(
   ;; General / Essential
   auto-completion
   git
   github
   helm
   syntax-checking

   ;; Languages / Major-modes
   clojure
   csv
   emacs-lisp
   ess                                  ; R
   html
   java
   javascript
   markdown
   nginx
   python ipython-notebook
   react
   ruby
   (scala :variables scala-enable-eldoc t)
   sql
   (typescript :variables
               typescript-fmt-on-save t)
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
