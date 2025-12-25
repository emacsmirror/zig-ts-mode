# Zig Tree Sitter Mode

Official repository is at CodeBerg:
[meow_king/zig-ts-mode](https://codeberg.org/meow_king/zig-ts-mode).

Target zig tree sitter grammar:
[github:tree-sitter-grammars/tree-sitter-zig](https://github.com/tree-sitter-grammars/tree-sitter-zig) (commit: 6479aa13f32f701c383083d8b28360ebd682fb7d)

Example configuration to manage Zig tree sitter garmmar:

```
(setq treesit-language-source-alist
      '((zig "https://github.com/tree-sitter-grammars/tree-sitter-zig")))
```

(Note, `M-x treesit-install-language-grammar` to install the Zig tree sitter grammar)


Tested on Emacs 30 and master branch.

---

Currently I don't write Zig. If you'd like to help maintain or take over this project, please contact me. Thank you!

# Installation

## From a version control (Emacs 30 or later)

```lisp
(use-package zig-ts-mode
  :vc (:url "https://codeberg.org/meow_king/zig-ts-mode"
       :rev :newest))
```

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
(add-to-list 'auto-mode-alist '("\\.zig\\(?:\\.zon\\)?\\'" . zig-ts-mode))
```
