;;; zig-ts-mode.el --- Tree Sitter support for Zig -*- lexical-binding: t; -*-

;; Copyright (C) 2023 meowking <mr.meowking@tutamail.com>

;; Version: 0.1.0
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
  '("asm" "defer" "errdefer" "test" "struct" "union" "enum" "opaque" "error"
    "async" "await" "suspend" "nosuspend" "resume"
    "fn"
    "and" "or" "orelse"
    "return"
    "if" "else" "switch"
    "for" "while" "break" "continue"
    "usingnamespace"
    "try" "catch"
    "const" "var" "volatile" "allowzero" "noalias"
    "addrspace" "align" "callconv" "linksection"
    "comptime" "export" "extern" "inline" "noinline" "packed" "pub"
    "threadlocal"))

(defconst zig-ts-mode--container-node-types
  '(;; paren
    "GroupedExpr" "SwitchExpr" "AsmExpr" "AsmOutputItem" "AsmInputItem"
    "WhileContinueExpr" "LinkSection" "AddrSpace" "CallConv" "IfPrefix"
    "WhilePrefix" "ForPrefix" "PrefixTypeOp" "FnCallArguments"
    "ContainerDeclType" "ByteAlign" "ParamDeclList"

    ;; brace
    "Block" "InitList" "ErrorSetDecl" "ContainerDecl"
    ;; "SwitchExpr"

    ;; bracket
    "SuffixOp" "SliceTypeStart" "PtrTypeStart" "ArrayTypeStart"
    ;; AsmOutputItem AsmInputItem
    
    "FnCallArguments"))

(defconst zig-ts-mode--container-node-types-regexp
  (regexp-opt zig-ts-mode--container-node-types))

(defvar zig-ts-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; comment
    (modify-syntax-entry  ?/     ". 12"  st)
    (modify-syntax-entry  ?\n    "> "     st)
    
    (modify-syntax-entry ?+   "."      st)
    (modify-syntax-entry ?-   "."      st)
    (modify-syntax-entry ?=   "."      st)
    (modify-syntax-entry ?*   "."      st)
    (modify-syntax-entry ?.   "."      st)
    (modify-syntax-entry ?%   "."      st)
    (modify-syntax-entry ?&   "."      st)
    (modify-syntax-entry ?|   "."      st)
    (modify-syntax-entry ?^   "."      st)
    (modify-syntax-entry ?!   "."      st)
    (modify-syntax-entry ?@   "."      st)
    (modify-syntax-entry ?~   "."      st)
    (modify-syntax-entry ?<   "."      st)
    (modify-syntax-entry ?>   "."      st)
    st))

(defun zig-ts-mode-comment-setup()
  "Setup comment related stuffs for `zig-ts-mode'."
  ;; stolen from `c-ts-common-comment-setup'
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx (seq "/" (+ "/") (* (syntax whitespace)))))
  (setq-local comment-end-skip (rx (* (syntax whitespace))
                                   (group (syntax comment-end)))))

(defvar zig-ts-mode-font-lock-feature-list
  '(( comment definition)
    ( keyword string)
    ( constant number type error builtin assignment)
    ( bracket function variable delimeter operator))
  "Font lock feature list for `zig-ts-mode'.")

(defvar zig-ts-mode-font-lock-settings
  (treesit-font-lock-rules
   ;; Zig Tree Sitter Font Lock
   :language 'zig
   :feature 'comment
   '([(container_doc_comment)
      (doc_comment)
      (line_comment)]
     @font-lock-comment-face)

   :language 'zig
   :feature 'string
   '([;; common
      (LINESTRING)
      (STRINGLITERALSINGLE)

      ;; special
      (CHAR_LITERAL)
      (EscapeSequence)
      (FormatSequence)]
     @font-lock-string-face)

   :language 'zig
   :feature 'keyword
   `([,@zig-ts-mode--keywords] @font-lock-keyword-face
     
     (BreakLabel (IDENTIFIER) @font-lock-keyword-face)
     (BlockLabel (IDENTIFIER) @font-lock-keyword-face))

   :language 'zig
   :feature 'variable
   '((AsmOutputItem variable: (IDENTIFIER) @font-lock-variable-use-face)
     (AsmInputItem variable: (IDENTIFIER) @font-lock-variable-use-face)
     (Payload variable: (IDENTIFIER) @font-lock-variable-use-face)
     (PtrPayload variable: (IDENTIFIER) @font-lock-variable-use-face)
     (PtrIndexPayload variable: (IDENTIFIER) @font-lock-variable-use-face)
     (PtrListPayload variable: (IDENTIFIER) @font-lock-variable-use-face)
     
     (SuffixExpr variable_type_function: (IDENTIFIER)
                 @font-lock-variable-use-face)

     (FieldOrFnCall field_access: (IDENTIFIER)
                    @font-lock-variable-use-face))
   

   :language 'zig
   :feature 'number
   '((INTEGER) @font-lock-number-face
     (FLOAT) @font-lock-number-face)

   :language 'zig
   :feature 'bracket
   '(["[" "]" "(" ")" "{" "}"] @font-lock-bracket-face
     
     (Payload "|" @font-lock-delimiter-face)
     (PtrPayload "|" @font-lock-delimiter-face)
     (PtrIndexPayload "|" @font-lock-delimiter-face)
     (PtrListPayload "|" @font-lock-delimiter-face))
   
   :language 'zig
   :feature 'delimeter
   '((FnProto exception: "!" @font-lock-delimiter-face)
     (ErrorUnionExpr exception: "!" @font-lock-delimiter-face)

     [ ";" "." "," ":" ] @font-lock-delimiter-face)

   :language 'zig
   :feature 'operator
   '([(CompareOp) (BitwiseOp) (BitShiftOp) (AdditionOp) (AssignOp)
      (MultiplyOp)
      (PrefixOp)
      "*" "**" "=>" ".?" ".*" "?"
      ".." "..."]
     @font-lock-operator-face
     
     (PtrTypeStart "c" @font-lock-builtin-face)  ; TODO example?
     )

   :language 'zig
   :feature 'assignment
   ;; We don't need to add `override' property here since
   ;; `variable' feature already contains more general rule (but it's at
   ;; feature level 4)
   '((AssignExpr
      :anchor
      (ErrorUnionExpr
       (SuffixExpr variable_type_function: (IDENTIFIER)
                   @font-lock-variable-use-face))
      :anchor
      (AssignOp)))

   :language 'zig
   :feature 'definition
   :override t
   '(;; function
     (FnProto function: (IDENTIFIER) @font-lock-function-name-face)

     ;; variable
     (VarDecl "var" variable_type_function: (IDENTIFIER)
              @font-lock-variable-name-face)
     (ContainerField (IDENTIFIER) @font-lock-variable-name-face)
     (ParamDecl parameter: (IDENTIFIER) @font-lock-variable-name-face)

     ;; This rule is not in upstream highlight.scm file
     (TestDecl (IDENTIFIER) @font-lock-function-name-face)

     ;; assume camelCase is a function
     ([(VarDecl variable_type_function: (IDENTIFIER) @font-lock-function-name-face)
       (ParamDecl parameter: (IDENTIFIER) @font-lock-function-name-face)]
      (:match "^[a-z]+\\([A-Z][a-z0-9]*\\)+$" @font-lock-function-name-face)))

   :language 'zig
   :feature 'constant  ; part 1
   :override t
   '((ContainerDecl
      (ContainerDeclType
       [(ErrorUnionExpr)
        "enum"])
      (ContainerField (IDENTIFIER) @font-lock-constant-face))

     ("." field_constant: (IDENTIFIER) @font-lock-constant-face)  ; TODO example
     (ErrorSetDecl field_constant: (IDENTIFIER) @font-lock-constant-face)
     
     (VarDecl "const" variable_type_function: (IDENTIFIER)
              @font-lock-constant-face)
     
     ;; assume all CAPS_1 is a constant
     ([(SuffixExpr variable_type_function: (IDENTIFIER)
                   @font-lock-constant-face)
       (FieldOrFnCall field_access: (IDENTIFIER) @font-lock-constant-face)]
      (:match "^[A-Z][A-Z_0-9]+$" @font-lock-constant-face)))


   :language 'zig
   :feature 'type
   :override t
   '(["anytype" (BuildinTypeExpr)] @font-lock-type-face
     
     ;; assume TitleCase is a type
     ([(VarDecl variable_type_function: (IDENTIFIER) @font-lock-type-face)
       (SuffixExpr variable_type_function: (IDENTIFIER) @font-lock-type-face)
       (ParamDecl parameter: (IDENTIFIER) @font-lock-type-face)
       (FieldOrFnCall field_access: (IDENTIFIER) @font-lock-type-face)]
      (:match "^[A-Z]\\([a-z]+[A-Za-z_0-9]*\\)*$" @font-lock-type-face)))
   
   :language 'zig
   :feature 'function
   :override t
   '((FieldOrFnCall function_call: (IDENTIFIER)
                    @font-lock-function-call-face)
     
     ;; assume camelCase is a function
     ([(SuffixExpr variable_type_function: (IDENTIFIER) @font-lock-function-call-face)
       (FieldOrFnCall field_access: (IDENTIFIER) @font-lock-function-call-face)]
      (:match "^[a-z]+\\([A-Z][a-z0-9]*\\)+$"
              @font-lock-function-call-face)))

   :language 'zig
   :feature 'constant  ; part 2
   :override t
   '((ContainerDecl
      (ContainerDeclType
       [(ErrorUnionExpr)
        "enum"])
      (ContainerField
       (ErrorUnionExpr
        (SuffixExpr (IDENTIFIER) @font-lock-constant-face)))))

   :language 'zig
   :feature 'builtin
   :override t
   '(["null" "unreachable" "undefined"] @font-lock-builtin-face

     [ "true" "false" ] @font-lock-builtin-face
     
     (BUILTINIDENTIFIER) @font-lock-builtin-face
     
     (((IDENTIFIER) @font-lock-builtin-face)
      (:equal @font-lock-builtin-face "_")))

   :language 'zig
   :feature 'error
   '((ERROR) @font-lock-warning-face)))

(defun zig-ts-mode--indentation-inside-container-nodes-p (_node parent _bol)
  "Whether the ancestor node(also itself) of PARENT is of container node type.
NODE, PARENT and BOL see `treesit-simple-indent-rules'."
  (treesit-parent-until
   ;; NODE can be nil (hit return), so we use PARENT
   parent
   (lambda (node)
     (string-match-p
      (concat "\\`" zig-ts-mode--container-node-types-regexp "\\'")
      (treesit-node-type node)))
   t))

(defun zig-ts-mode--indentation-ancestor-container-nodes-bol (node parent bol)
  "Return the beginning of line position of the closest ancestor container node.
NODE, PARENT and BOL see `treesit-simple-indent-rules'."
  (save-excursion
    (goto-char
     (treesit-node-start
      (zig-ts-mode--indentation-inside-container-nodes-p node parent bol)))
    (back-to-indentation)
    (point)))

(defun zig-ts-mode--indentation-prev-line-is-closed-FieldOrFnCall-p
    (_node _parent _bol)
  "Whether the previous line is FieldOrFnCall node.
Ignore comments.
NODE, PARENT and BOL see `treesit-simple-indent-rules'."
  (save-excursion
    (forward-line -1)
    (while (string-match-p
            "comment"  ; all comment node types
            (treesit-node-type (treesit-node-at (point))))
      (forward-line -1))
    (when (progn
            (end-of-visible-line)
            (not (equal (char-before (point)) ?\;)))
      (back-to-indentation)
      (let* ((node (treesit-node-at (point)))
             (node-type (treesit-node-type node))
             (parent (treesit-node-parent node))
             (parent-type (treesit-node-type parent)))
        (or
         ;; assume user don't write too loose syntax like
         ;; Foo
         ;;   .
         ;;   FnCall()
         
         ;; multi-line
         (and
          (equal node-type ")")
          (equal parent-type "FnCallArguments")
          (equal
           (treesit-node-type (treesit-node-parent parent))
           "FieldOrFnCall"))
         ;; one line
         (and
          (equal node-type ".")
          (equal parent-type "FieldOrFnCall")
          (if (<= (treesit-node-child-count parent) 2)
              t
            ;; If it is an FnCall, then check whether it ends at this line
            (end-of-visible-line)
            (equal (char-before (point)) ?\))))))
      )))

(defun zig-ts-mode--indentation-prev-non-comment-line-bol
    (_node _parent _bol)
  "Return the beginning of line position of the previous non-comment-line.
NODE, PARENT and BOL see `treesit-simple-indent-rules'."
  (save-excursion
    (forward-line -1)
    (while (string-match-p
            "comment"  ; all comment node types
            (treesit-node-type (treesit-node-at (point))))
      (forward-line -1))
    (back-to-indentation)
    (point)))


(defconst zig-ts-mode--indentation-dot-or-comment-node-type-regexp
  (rx (or (seq bos "." eos) (seq "comment" eos))))

(defconst zig-ts-mode--indentation-assignment-node-type-regexp
  (rx bos (or "VarDecl" "FieldInit") eos))

(defun zig-ts-mode--indentation-parent-until-assignment-node-bol
    (_node parent _bol)
  "Find the beginning of line position of the specific assignment ancestor node.
NODE, PARENT and BOL see `treesit-simple-indent-rules'."
  (save-excursion
    (goto-char
     (treesit-node-start
      (treesit-parent-until
       parent
       zig-ts-mode--indentation-assignment-node-type-regexp
       t)))
    (back-to-indentation)
    (point)))

(defvar zig-ts-mode-indent-rules
  `((zig
     ;; ((lambda (node parent bol)
     ;;    (message "%s: %s %s %s %s %s"
     ;;             (point) node parent
     ;;             (treesit-node-parent parent)
     ;;             (treesit-node-parent (treesit-node-parent parent)) bol)
     ;;    nil)
     ;;  parent-bol 0)

     ((parent-is "source_file") column-0 0)
     ((node-is ,(regexp-opt '(")" "]" "}"))) parent-bol 0)
     
     ((parent-is "comment") prev-adaptive-prefix 0)

     ;; Example
     ;; const str =
     ;;     \\ hello
     ;; ;
     ((node-is "\\`;\\'") parent-bol 0)

     ((node-is "FieldOrFnCall") parent-bol zig-ts-mode-indent-offset)
     ((and (or no-node
               (node-is
                ,zig-ts-mode--indentation-dot-or-comment-node-type-regexp))
           zig-ts-mode--indentation-prev-line-is-closed-FieldOrFnCall-p)
      zig-ts-mode--indentation-prev-non-comment-line-bol 0)
     
     ((node-is "else") parent-bol 0)
     ((parent-is ,(rx bos (or "IfStatement" "IfExpr") eos))
      standalone-parent zig-ts-mode-indent-offset)

     ((parent-is ,(rx bos (or "ForStatement" "ForExpr") eos))
      standalone-parent zig-ts-mode-indent-offset)

     ((parent-is ,(rx bos (or "WhileStatement" "WhileExpr") eos))
      standalone-parent zig-ts-mode-indent-offset)

     
     ;; Multi-line String
     ;; We use `zig-ts-mode--indentation-parent-until-assignment-node-bol'
     ;; instead of `standalone-parent' or `great-grand-parent' here considering
     ;; the following example:
     ;; pub const description =
     ;;    \\ el
     ;;    \\ psy
     ;; ;
     ((parent-is ,zig-ts-mode--indentation-assignment-node-type-regexp)
      zig-ts-mode--indentation-parent-until-assignment-node-bol
      zig-ts-mode-indent-offset)  ; this rules also works for others
     ((node-is "\\`LINESTRING\\'")
      zig-ts-mode--indentation-parent-until-assignment-node-bol
      zig-ts-mode-indent-offset)

     ;; FnCallArguments often appears as much far away from node as its
     ;; ancestor, so here we use custom function
     (zig-ts-mode--indentation-inside-container-nodes-p
      zig-ts-mode--indentation-ancestor-container-nodes-bol
      zig-ts-mode-indent-offset)))
  "Tree-sitter indent rules for `zig-ts-mode'.")


;;;###autoload
(defvar-keymap zig-ts-mode-map)

(defun zig-ts-mode--imenu-fn-pred-fn (node)
  "Test whether the given function NODE is validated.
See `treesit-simple-iemnu-settings'."
  (if (equal (treesit-node-type node) "FnProto")
      t
    ;; VarDecl
    ;; assume camelCase is a function
    (let ((case-fold-search nil))
      (string-match-p
       "^[a-z]+\\([A-Z][a-z0-9]*\\)+$"
       (treesit-node-text
        (treesit-node-child-by-field-name node "variable_type_function"))))))

(defun zig-ts-mode--imenu-fn-name-fn (node)
  "Return appropriate name for the given function NODE.
See `treesit-simple-iemnu-settings'."
  (if (equal (treesit-node-type node) "FnProto")
      (treesit-node-text
       (treesit-node-child-by-field-name node "function"))
    (treesit-node-text
     (treesit-node-child-by-field-name node "variable_type_function"))))

(defun zig-ts-mode--iemnu-enum-name-fn (node)
  "Return appropriate name for the given enum NODE.
See `treesit-simple-iemnu-settings'."
  (let ((ggggp-node (treesit-node-parent
                     (treesit-node-parent
                      (treesit-node-parent
                       (treesit-node-parent
                        (treesit-node-parent
                         node)))))))
    (when (equal (treesit-node-type ggggp-node) "VarDecl")
      (treesit-node-text
       (treesit-node-child-by-field-name ggggp-node
                                         "variable_type_function")))))

(defun zig-ts-mode--imenu-test-name-fn (node)
  "Return appropriate name for the given test NODE.
See `treesit-simple-iemnu-settings'."
  (treesit-node-text (treesit-node-child node 1)))

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
    ("TestDecl"
     (treesit-node-text
      (treesit-node-child node 1)))
    ("Decl"
     (let ((child (treesit-node-child node 0)))
       (pcase (treesit-node-type child)
         ("FnProto"
          (treesit-node-text
           (treesit-node-child-by-field-name child "function")))
         ("VarDecl"
          (treesit-node-text
           (treesit-node-child-by-field-name
            child
            "variable_type_function"))))))))


;;;###autoload
(define-derived-mode zig-ts-mode prog-mode "Zig-ts"
  "Major mode for editing Zig, powered by tree-sitter."
  :group 'zig-ts
  :syntax-table zig-ts-mode-syntax-table

  (unless (treesit-ready-p 'zig)
    (user-error "Tree-sitter for Zig isn't available"))
  
  (setq-local treesit-primary-parser (treesit-parser-create 'zig))

  ;; Comments.
  (zig-ts-mode-comment-setup)

  ;; Electric.
  (setq-local electric-indent-chars
              (append "{}().,;" electric-indent-chars))
  
  ;; Font-lock.
  (setq-local treesit-font-lock-settings zig-ts-mode-font-lock-settings)
  (setq-local treesit-font-lock-feature-list zig-ts-mode-font-lock-feature-list)

  ;; Indentation
  (setq-local treesit-simple-indent-rules zig-ts-mode-indent-rules)

  ;; Imenu.
  ;; NOTE Type and Enum may have collision, also Constant <-> Type <-> Fn
  ;; TODO add struct
  (setq-local treesit-simple-imenu-settings
              `(("Constant" "\\`VarDecl\\'" zig-ts-mode--iemnu-constant-pred-fn
                 zig-ts-mode--imenu-constant-name-fn)
                ("Type" "\\`VarDecl\\'" zig-ts-mode--iemnu-type-pred-fn
                 zig-ts-mode--imenu-type-name-fn)
                ("Fn" ,(rx bos (or "FnProto" "VarDecl") eos)
                 zig-ts-mode--imenu-fn-pred-fn zig-ts-mode--imenu-fn-name-fn)
                ("Enum" "\\`enum\\'" nil zig-ts-mode--iemnu-enum-name-fn)
                ("Test" "\\`TestDecl\\'" nil zig-ts-mode--imenu-test-name-fn)))

  ;; Navigation.
  (setq-local treesit-defun-type-regexp
              (rx bos (or "Decl" "TestDecl") eos))
  (setq-local treesit-defun-name-function #'zig-ts-mode--defun-name)

  (treesit-major-mode-setup))

(add-to-list 'auto-mode-alist '("\\.zig\\(?:\\.zon\\)?\\'" . zig-ts-mode))

(provide 'zig-ts-mode)

;;; zig-ts-mode.el ends here
