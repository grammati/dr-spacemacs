## Things we don't like about spacemacs, that we should try to fix

- Magit: no binding for magit-discard on a file
  - Works in develop, it's `x`
- `SPC t E` - can set editing style to "emacs" or "hybrid", but not "vim"
- Does not respect split-width-threshold / split-height-threshold
  - The `popwin` package is to blame. Can effectively disable it in user config.
- No binding for join-line? I use that a lot.
  - It's `J`, and it works great
- `SPC t C d` toggles rainbow-delimiters, but only for current file. Should be global?
- It seems like maybe some buffers should be created in insert mode: cider repl, git commit, ...? Or should we just add a hook?
- When using M-m as the leader-key most command names come up as +prefix. This makes discoverability of keybindings/commands really difficult if you're trying to use holy-mode, or in insert-mode of hybrid.
- No multiple-cursors! That sucks. But evil-mc is in development.
- Completion - neither RET nor C-j works when you want to just get a newline (eg: Type "foo" when the candidates are "foobar" "foobaz")
