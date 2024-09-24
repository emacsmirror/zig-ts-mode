# Zig Tree Sitter Mode

Official repository is at CodeBerg:
[meow_king/zig-ts-mode](https://codeberg.org/meow_king/zig-ts-mode).

Target zig tree sitter grammar:
[github:maxxnino/tree-sitter-zig](https://github.com/maxxnino/tree-sitter-zig)

# Installation

## Elpaca

``` lisp
(use-package zig-ts-mode
  :ensure (:type git :host codeberg :repo "meow_king/zig-ts-mode"))
```

## Straight

``` lisp
(use-package zig-ts-mode
  :ensure '(:type git :host codeberg :repo "meow_king/zig-ts-mode"))
```

# Configuration

Automatically enter `zig-ts-mode`:

``` lisp
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode))
```
