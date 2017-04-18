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
   emacs-lisp
   ess                                  ; R
   (go :variables
       ;;go-use-gometalinter t
       go-tab-width 4)
   html
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
