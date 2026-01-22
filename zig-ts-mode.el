;;; zig-ts-mode.el --- Tree Sitter support for Zig -*- lexical-binding: t; -*-

;; Copyright (C) 2023 meowking <mr.meowking@tutamail.com>

;; Version: 0.2.0
;; Author: meowking <mr.meowking@tutamail.com>
;; Keywords: zig languages tree-sitter
;; URL: https://codeberg.org/meow_king/zig-ts-mode
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'treesit)

(defgroup zig-ts nil
  "Tree Sitter support for Zig."
  :link '(url-link "https://ziglang.org/")
  :prefix "zig-ts"
  :group 'languages)

(defcustom zig-ts-mode-indent-offset 4
  "Indent Zig code by this number of spaces."
  :type 'natnum
  :safe 'natnump
  :group 'zig-ts)

(defconst zig-ts-mode--keywords
  '("asm" "defer" "errdefer" "test" "error" "const" "var"
    "struct" "union" "enum" "opaque"
    "async" "await" "suspend" "nosuspend" "resume"
    "fn"
    "and" "or" "orelse"
    "if" "else" "switch"
    "for" "while" "break" "continue"
    "usingnamespace" "export"
    "try" "catch"
    "volatile" "allowzero" "noalias" "addrspace" "align" "callconv" "linksection" "pub"
    "inline" "noinline" "extern" "comptime" "packed" "threadlocal"))


(defconst zig-ts-mode--operators
  '("=" "*=" "*%=" "*|=" "/=" "%=" "+=" "+%=" "+|=" "-=" "-%=" "-|=" "<<=" "<<|=" ">>="
    "&=" "^=" "|=" "!" "~" "-" "-%" "&" "==" "!=" ">" ">=" "<=" "<" "&" "^" "|" "<<" ">>"
    "<<|" "+" "++" "+%" "-%" "+|" "-|" "*" "/" "%" "**" "*%" "*|" "||" ".*" ".?" "?" ".."
    "..."))


(defconst zig-ts-mode--indent-forward-node-types-regexp
  (rx
   (or
    (seq string-start
         (or "block" "variable_declaration" "container_field")
         string-end)
    "statement"
    "expression")))



(defvar zig-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments: Zig only has // comments
    ;; 12 means: start of a 2-char comment sequence (1st and 2nd char are the same)
    (modify-syntax-entry ?/   ". 12" table)
    (modify-syntax-entry ?\n  ">"    table)

    ;; Strings and Chars
    (modify-syntax-entry ?\"  "\""   table)
    (modify-syntax-entry ?\'  "\""   table)
    (modify-syntax-entry ?\\  "\\"   table) ;; Escape character

    ;; Symbol constituents (variable names)
    (modify-syntax-entry ?_   "_"    table)

    ;; Punctuation / Operators
    ;; These are mostly "." by default, but being explicit is good practice
    (modify-syntax-entry ?+   "."    table)
    (modify-syntax-entry ?-   "."    table)
    (modify-syntax-entry ?=   "."    table)
    (modify-syntax-entry ?%   "."    table)
    (modify-syntax-entry ?&   "."    table)
    (modify-syntax-entry ?|   "."    table)
    (modify-syntax-entry ?^   "."    table)
    (modify-syntax-entry ?!   "."    table)
    (modify-syntax-entry ?@   "."    table)
    (modify-syntax-entry ?~   "."    table)
    (modify-syntax-entry ?<   "."    table)
    (modify-syntax-entry ?>   "."    table)
    (modify-syntax-entry ?*   "."    table) ;; Distinct from Rust (no block comments)
    (modify-syntax-entry ?.   "."    table)

    table)
  "Syntax table for `zig-ts-mode'.")

(defun zig-ts-mode-comment-setup()
  "Setup comment related stuffs for `zig-ts-mode'."
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  
  (setq-local comment-start-skip (rx (seq "/" (+ "/") (* (syntax whitespace)))))
  (setq-local comment-end-skip (rx (* (syntax whitespace))
                                   (group (syntax comment-end))))

  (setq-local adaptive-fill-mode t)
  (setq-local paragraph-start
              (rx (or (seq (* (syntax whitespace))
                           (group (seq "/" (+ "/")))
                           (* (syntax whitespace))
                           eol))
                  "\f"))
  
  (setq-local paragraph-separate paragraph-start)
  
  ;; This ensures that when you press M-j inside a comment, it inserts `//` 
  ;; on the next line.
  (setq-local comment-line-break-function #'comment-indent-new-line)
  (setq-local comment-multi-line t))

(defun zig-ts-mode--comment-docstring (node override start end &rest _args)
  "Use the comment or documentation face appropriately for comments."
  (let* ((beg (treesit-node-start node))
         (face (save-excursion
                 (goto-char beg)
                 (if (looking-at-p
                      "^//!")
                     'font-lock-doc-face
                   'font-lock-comment-face))))
    (treesit-fontify-with-override beg (treesit-node-end node)
                                   face override start end)))



(defvar zig-ts-mode--font-lock-feature-list
  '(( comment definition)
    ( keyword string)
    ( builtin constant escape-sequence label number type)
    ( bracket delimiter function variable operator error)
    )
  "Font lock feature list for `zig-ts-mode'.")

(defvar zig-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'zig
   :feature 'comment
   '((comment) @zig-ts-mode--comment-docstring)

   :language 'zig
   :feature 'definition
   '((function_declaration name: (identifier) @font-lock-function-name-face)
     (variable_declaration "const" (identifier) @font-lock-constant-face)
     (variable_declaration :anchor (identifier) @font-lock-variable-name-face))

   :language 'zig
   :feature 'keyword
   `([,@zig-ts-mode--keywords] @font-lock-keyword-face)

   :language 'zig
   :feature 'string
   '([(character)
      (string)
      (multiline_string)]
     @font-lock-string-face)
   

   :language 'zig
   :feature 'builtin
   '([(builtin_identifier) "c"] @font-lock-builtin-face
     (calling_convention "(" _ @font-lock-builtin-face ")"))

   :language 'zig
   :feature 'constant
   '([(boolean)"null" "unreachable" "undefined"] @font-lock-constant-face
     (field_expression "." member: (identifier) @font-lock-constant-face))

   :language 'zig
   :feature 'label
   '((block_label (identifier) @font-lock-constant-face)
     (break_label (identifier) @font-lock-constant-face))

   :language 'zig
   :feature 'number
   '([(integer) (float)] @font-lock-number-face)

   :language 'zig
   :feature 'type
   '([(parameter type: (identifier) @font-lock-type-face)]
     [(builtin_type) "anyframe"] @font-lock-type-face)

   :language 'zig
   :feature 'bracket
   '(["[" "]" "(" ")" "{" "}"] @font-lock-bracket-face
     (payload "|" @font-lock-bracket-face))

   :language 'zig
   :feature 'delimiter
   '([";" "." "," ":" "=>" "->"] @font-lock-delimiter-face)

   :language 'zig
   :feature 'function
   '((call_expression function: (identifier) @font-lock-function-call-face)
     (call_expression
      function: (field_expression member: (identifier) @font-lock-function-call-face)))

   :language 'zig
   :feature 'variable
   '((field_initializer "." (identifier) @font-lock-variable-use-face)
     (field_expression (_) member: (identifier) @font-lock-variable-use-face)
     (container_field name: (identifier) @font-lock-variable-use-face)
     (identifier) @font-lock-variable-use-face)
   

   :language 'zig
   :feature 'operator
   `([,@zig-ts-mode--operators] @font-lock-operator-face)


   ;; Overrides ================================================================
   :language 'zig
   :feature 'type
   :override t
   '((enum_declaration (container_field type: (identifier) @font-lock-type-face)))
   
   :language 'zig
   :feature 'variable
   :override t
   '((initializer_list
      (assignment_expression
       left: (field_expression "." member: (identifier) @font-lock-variable-use-face))))
   
   :language 'zig
   :feature 'builtin
   :override t
   '((((identifier) @font-lock-builtin-face)
      (:equal "_" @font-lock-builtin-face)))
   

   :language 'zig
   :feature 'escape-sequence
   :override t
   '((escape_sequence) @font-lock-escape-face)

   :language 'zig
   :feature 'constant
   :override t
   '(((identifier) @font-lock-constant-face
      (:match "^[A-Z][A-Z_0-9]+$" @font-lock-constant-face)))

   :language 'zig
   :feature 'type
   :override t
   '(((identifier) @font-lock-type-face
      (:match "^[A-Z_][a-zA-Z0-9_]*" @font-lock-type-face)))

   :language 'zig
   :feature 'type
   :override t
   '((variable_declaration
      (identifier) @font-lock-type-face
      "="
      [(struct_declaration) (enum_declaration)
       (union_declaration) (opaque_declaration)]))

   :language 'zig
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face)
   ))

(defun zig-ts-mode--indentation-inside-indenter-nodes-p (_node parent _bol)
  "Whether the ancestor node(also itself) of PARENT is of indenter node type.
NODE, PARENT and BOL see `treesit-simple-indent-rules'."
  (treesit-parent-until
   ;; NODE can be nil (hit return), so we use PARENT
   parent
   (lambda (node)
     (string-match-p
      (concat "\\`" zig-ts-mode--indent-forward-node-types-regexp "\\'")
      (treesit-node-type node)))
   t))

(defun zig-ts-mode--indentation-ancestor-indenter-nodes-bol (node parent bol)
  "Return the beginning of line position of the closest ancestor indenter node.
NODE, PARENT and BOL see `treesit-simple-indent-rules'."
  (save-excursion
    (goto-char
     (treesit-node-start
      (zig-ts-mode--indentation-inside-indenter-nodes-p node parent bol)))
    (back-to-indentation)
    (point)))


(defvar zig-ts-mode--indent-rules
  `((zig
     ((lambda (node parent bol)
        (message "%s: %s %s %s %s %s"
                 (point) node parent
                 (treesit-node-parent parent)
                 (treesit-node-parent (treesit-node-parent parent)) bol)
        nil)
      parent-bol 0)
     
     ((parent-is "source_file") column-0 0)
     ((node-is ,(regexp-opt '(")" "]" "}"))) parent-bol 0)
     ((parent-is "comment") prev-adaptive-prefix 0)

     ((node-is "\\`;\\'") parent-bol 0)
     ((node-is "else") parent-bol 0)
     ((parent-is "multiline_string") prev-line 0)

     (zig-ts-mode--indentation-inside-indenter-nodes-p
      zig-ts-mode--indentation-ancestor-indenter-nodes-bol
      zig-ts-mode-indent-offset)
     )))


;;;###autoload
(defvar-keymap zig-ts-mode-map)

(defun zig-ts-mode--imenu-fn-pred-fn (node)
  "Test whether the given function NODE is validated.
See `treesit-simple-iemnu-settings'."
  (if (equal (treesit-node-type node) "function_declaration")
      t
    ;; VarDecl
    ;; assume camelCase is a function
    (let ((case-fold-search nil))
      (string-match-p
       "^[a-z]+\\([A-Z][a-z0-9]*\\)+$"
       (treesit-node-text
        (treesit-node-child-by-field-name node "variable_type_function"))))))

(defun zig-ts-mode--imenu-func-name-fn (node)
  "Return appropriate name for the given function NODE.
See `treesit-simple-iemnu-settings'."
  (treesit-node-text (treesit-node-child-by-field-name node "name")))

(defun zig-ts-mode--imenu-assign-style-declaration-name-fn (node)
  "Return appropriate name for the given enum NODE.
See `treesit-simple-iemnu-settings'."
  (let ((parent-node (treesit-node-parent node)))
    (when (equal (treesit-node-type parent-node) "variable_declaration")
      (treesit-node-text (treesit-node-child parent-node 0 t)))))

(defun zig-ts-mode--imenu-test-name-fn (node)
  "Return appropriate name for the given test NODE.
See `treesit-simple-iemnu-settings'."
  (treesit-node-text (treesit-node-child node 0 t)))

(defun zig-ts-mode--iemnu-type-pred-fn (node)
  "Test whether the given type NODE is validated.
See `treesit-simple-iemnu-settings'."
  ;; assume TitleCase is a type
  (let ((case-fold-search nil))
    (string-match-p
     "^[A-Z]\\([a-z]+[A-Za-z_0-9]*\\)*$"
     (treesit-node-text
      (treesit-node-child-by-field-name node "variable_type_function")))))

(defun zig-ts-mode--imenu-type-name-fn (node)
  "Return appropriate name for the given type NODE.
See `treesit-simple-iemnu-settings'."
  (treesit-node-text
   (treesit-node-child-by-field-name node "variable_type_function")))

(defun zig-ts-mode--iemnu-constant-pred-fn (node)
  "Test whether the given constant NODE is validated.
See `treesit-simple-iemnu-settings'."
  ;; assume TitleCase is a type
  (let ((case-fold-search nil))
    (and
     (equal
      (treesit-node-text (treesit-node-child node 0))
      "const")
     ;; assume CAPS_1 is a constant
     (string-match-p
      "^[A-Z][A-Z_0-9]+$"
      (treesit-node-text
       (treesit-node-child-by-field-name node "variable_type_function"))))))

(defun zig-ts-mode--imenu-constant-name-fn (node)
  "Return appropriate name for the given constant NODE.
See `treesit-simple-iemnu-settings'."
  (treesit-node-text
   (treesit-node-child-by-field-name node "variable_type_function")))

(defun zig-ts-mode--defun-name (node)
  (pcase (treesit-node-type node)
    ("function_declaration" (zig-ts-mode--imenu-func-name-fn node))
    (_ (zig-ts-mode--imenu-assign-style-declaration-name-fn node))))


;;;###autoload
(define-derived-mode zig-ts-mode prog-mode "Zig"
  "Major mode for editing Zig, powered by tree-sitter."
  :group 'zig-ts
  :syntax-table zig-ts-mode--syntax-table

  (unless (if (>= emacs-major-version 31)
              (treesit-ensure-installed 'zig)
            (treesit-ready-p 'zig))
    (user-error "Tree-sitter for Zig isn't available"))
  
  (setq-local treesit-primary-parser (treesit-parser-create 'zig))


  ;; Comments.
  (zig-ts-mode-comment-setup)
  
  ;; Font-lock.
  (setq-local treesit-font-lock-settings zig-ts-mode--font-lock-settings)
  (setq-local treesit-font-lock-feature-list zig-ts-mode--font-lock-feature-list)
  
  ;; Indentation
  (setq-local electric-indent-chars
              (append "{}()[].,;" electric-indent-chars))
  (setq-local treesit-simple-indent-rules zig-ts-mode--indent-rules)

  (setq-local
   treesit-simple-imenu-settings
   `(("Fn" "\\`function_declaration\\'" nil zig-ts-mode--imenu-func-name-fn)
     ("Enum" "\\`enum_declaration\\'"
      nil zig-ts-mode--imenu-assign-style-declaration-name-fn)
     ("Struct" "\\`struct_declaration\\'"
      nil zig-ts-mode--imenu-assign-style-declaration-name-fn)
     ("Opaque" "\\`opaque_declaration\\'"
      nil zig-ts-mode--imenu-assign-style-declaration-name-fn)
     ("ErrorSet" "\\`error_set_declaration\\'"
      nil zig-ts-mode--imenu-assign-style-declaration-name-fn)
     ("Union" "\\`union_declaration\\'"
      nil zig-ts-mode--imenu-assign-style-declaration-name-fn)
     ("Test" "\\`test_declaration\\'" nil zig-ts-mode--imenu-test-name-fn)))

  (setq-local treesit-defun-type-regexp
              (regexp-opt '("declaration")))

  (setq-local treesit-defun-name-function #'zig-ts-mode--defun-name)
  (treesit-major-mode-setup))

(add-to-list 'auto-mode-alist '("\\.zig\\(?:\\.zon\\)?\\'" . zig-ts-mode))

(provide 'zig-ts-mode)

;;; zig-ts-mode.el ends here
