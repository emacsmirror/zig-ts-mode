;;; zig-ts-mode.el --- Tree Sitter support for Zig -*- lexical-binding: t; -*-

;; Copyright (C) 2023 meowking <mr.meowking@tutamail.com>

;; Version: 0.1.0
;; Author: meowking <mr.meowking@tutamail.com>
;; Keywords: zig languages tree-sitter
;; URL: https://codeberg.org/meow_king/zig-ts-mode
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "29.4"))

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
  :type 'integer
  :group 'zig-ts)

(defvar zig-ts-mode--keywords
  '("addrspace" "align" "allowzero" "and" "anyframe" "anytype" "asm" "async"
    "await" "break" "callconv" "catch" "comptime" "const" "continue" "defer"
    "else" "enum" "errdefer" "error" "export" "extern" "fn" "for" "if" "inline"
    "noalias" "nosuspend" "noinline" "opaque" "or" "orelse" "packed" "pub"
    "resume" "return" "linksection" "struct" "suspend" "switch" "test"
    "threadlocal" "try" "union" "unreachable" "usingnamespace" "var" "volatile"
    "while"))

(defvar zig-ts-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; comment
    (modify-syntax-entry  ?/     ". 12"  st)
    (modify-syntax-entry  ?\n    "> "     st)
    st))

(defun zig-ts-mode-comment-setup()
  "Setup comment related stuffs for `typst-ts-mode'."
  ;; stolen from `c-ts-common-comment-setup'
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx (seq "/" (+ "/") (* (syntax whitespace)))))
  (setq-local comment-end-skip (rx (* (syntax whitespace))
                                   (group (syntax comment-end)))))

(defvar zig-ts-mode-font-lock-feature-list
  ;; '(( comment definition)
  ;;   ( keyword string)
  ;;   ( assignment attribute builtin constant escape-sequence
  ;;     number type)
  ;;   ( bracket delimiter error function operator property variable)))
  '(( comment definition)
    ( keyword)
    ( number type error)
    ( function variable)))

(defvar zig-ts-mode-font-lock-rules nil
  "TODO doc.")

(defvar zig-ts-mode-font-lock-rules-error nil
  "Customize font lock feature `error'.")

(defvar zig-ts-mode-font-lock-rules-comment nil
  "Customize font lock feature `comment'.")

(defvar zig-ts-mode-font-lock-rules-definition nil
  "Customize font lock feature `definition'.")

(defvar zig-ts-mode-font-lock-rules-function nil
  "Customize font lock feature `function'.")

(defvar zig-ts-mode-font-lock-rules-variable nil
  "Customize font lock feature `variable'.")

(defvar zig-ts-mode-font-lock-rules-brackets nil
  "Customize font lock feature `brackets'.")

(defvar zig-ts-mode-font-lock-rules-builtin nil
  "Customize font lock feature `builtin'.")

(defvar zig-ts-mode-font-lock-rules-keyword nil
  "Customize font lock feature `keyword'.")

(defvar zig-ts-mode-font-lock-rules-number nil
  "Customize font lock feature `number'.")

(defvar zig-ts-mode-font-lock-rules-type nil
  "Customize font lock feature `type'.")

(defun zig-ts-mode-font-lock-rules ()
  "TODO doc."
  `(;; Zig Tree Sitter Font Lock
    :language zig
    :feature comment
    ,(if zig-ts-mode-font-lock-rules-comment
         zig-ts-mode-font-lock-rules-comment
       '([(container_doc_comment)
          (doc_comment)
          (line_comment)]
         @font-lock-comment-face))

    :language zig
    :feature definition
    ,(if zig-ts-mode-font-lock-rules-definition
         zig-ts-mode-font-lock-rules-definition
       '(;; function
         (FnProto function: (IDENTIFIER) @font-lock-function-name-face)

         ;; variable
         (VarDecl variable_type_function: (IDENTIFIER)
                  @font-lock-variable-name-face)
         (ContainerField field_member: (IDENTIFIER)
                         @font-lock-variable-name-face)
         (ParamDecl parameter: (IDENTIFIER) @font-lock-variable-name-face)))

    :language zig
    :feature keyword
    ,(if zig-ts-mode-font-lock-rules-keyword
         zig-ts-mode-font-lock-rules-keyword
       `([,@zig-ts-mode--keywords] @font-lock-keyword-face))

    :language zig
    :feature type
    ,(if zig-ts-mode-font-lock-rules-type
         zig-ts-mode-font-lock-rules-type
       '((BuildinTypeExpr) @font-lock-type-face))
    
    :language zig
    :feature function
    ,(if zig-ts-mode-font-lock-rules-function
         zig-ts-mode-font-lock-rules-function
       '((FieldOrFnCall function_call: (IDENTIFIER)
                        @font-lock-function-call-face)))

    :language zig
    :feature variable
    ,(if zig-ts-mode-font-lock-rules-variable
         zig-ts-mode-font-lock-rules-variable
       '((AsmOutputItem variable: (IDENTIFIER) @font-lock-variable-use-face)
         (AsmInputItem variable: (IDENTIFIER) @font-lock-variable-use-face)
         (Payload variable: (IDENTIFIER) @font-lock-variable-use-face)
         (PtrPayload variable: (IDENTIFIER) @font-lock-variable-use-face)
         (PtrIndexPayload variable: (IDENTIFIER) @font-lock-variable-use-face)
         (PtrListPayload variable: (IDENTIFIER) @font-lock-variable-use-face)
         
         (SuffixExpr variable_type_function: (IDENTIFIER)
                     @font-lock-variable-use-face)

         (FieldOrFnCall field_access: (IDENTIFIER)
                        @font-lock-variable-use-face)))
    

    :language zig
    :feature number
    ,(if zig-ts-mode-font-lock-rules-number
         zig-ts-mode-font-lock-rules-number
       '((INTEGER) @font-lock-number-face
         (FLOAT) @font-lock-number-face))

    :language zig
    :feature brackets
    ,(if zig-ts-mode-font-lock-rules-brackets
         zig-ts-mode-font-lock-rules-brackets
       '(["[" "]" "(" ")" "{" "}"
          (Payload "|")
          (PtrPayload "|")
          (PtrIndexPayload "|")]
         @font-lock-bracket-face))

    :language zig
    :feature builtin
    ,(if zig-ts-mode-font-lock-rules-builtin
         zig-ts-mode-font-lock-rules-builtin
       ;; FIXME
       '(["null" "unreachable" "undefined"  ; constant
          "anytype" (BuildinTypeExpr)
          
          ]  ; type
         @font-lock-builtin-face
         ))

    :language zig
    :feature error
    ,(if zig-ts-mode-font-lock-rules-error
         zig-ts-mode-font-lock-rules-error
       '((ERROR) @font-lock-warning-face))
    ))


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
  
  ;; Font-lock.
  (setq-local treesit-font-lock-settings
              (apply #'treesit-font-lock-rules (if zig-ts-mode-font-lock-rules
                                                   zig-ts-mode-font-lock-rules
                                                 (zig-ts-mode-font-lock-rules))))
  (setq-local treesit-font-lock-feature-list zig-ts-mode-font-lock-feature-list)

  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-ts-mode))

(provide 'zig-ts-mode)

;;; zig-ts-mode.el ends here
