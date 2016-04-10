This layer adds evil-friendly structural editing commands to enhance lisp
coding. It allows using powerful editing commands in `evil-lispy-mode`.

You can enter this mode with the following commands:

```
  (  jump to the previous parenthesis and enter evil-lispy-mode
  )  same, but jump to the next parenthesis instead

  gv select current symbol and enter evil-lispy-mode

  <i or <I  go to the start of the current expression, insert a space and enter
            evil-insert-state
  >i or >I  same but jump to the end of the current expression instead
```

If you are familiar with lispy-mode, you should know this layer changes some of
the keybindings to be more evil-like:

```
  o  switch to the other side of the current expression (like o in visual mode)
  i  go "inside" the current expression or jump to the next expression in this direction

  f  format the current expression
  d  move the current expression relative to its neighbors
```
