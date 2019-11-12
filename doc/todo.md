## dr-spacemacs TODOs

### Major Issues, Things we miss from rally-emacs

- No multiple-cursors! That sucks. evil-mc is in development, and might help.

- Clojure: too easy to mess up parens in normal-mode, becuase paredit has no power there. eg: just hit `x` at the wrong time by accident and you have unbalanced parens! Often I don't even notice until later, when it won't compile.

- Clojure: I really miss the automatic save-and-eval.

- Search: with rally-emacs, you can do `C-x C-a` or `C-x C-g` (ag-project or projectile-ag), then go through all the results with next-error / previous-error (F6 / F7). There is nothing as conventientin spacemacs.


### Annoyances

- It seems like maybe some buffers should be created in insert mode: cider repl, git commit, ...? Or should we just add a hook?

- `SPC t C d` toggles rainbow-delimiters, but only for current file. Should be global?

- Completion - neither RET nor C-j works when you want to just get a newline (eg: Type "foo" when the candidates are "foobar" "foobaz")

- Magit: magit has really nice keybingings already. Spacemacs just makes them worse.

- cider-debug: keybindings don't work unless you go into insert mode

- I would love to have a more obvious visual indication of insert vs. normal mode.

### Resolved

- Does not respect split-width-threshold / split-height-threshold
  - The `popwin` package is to blame. Can effectively disable it in user config.

