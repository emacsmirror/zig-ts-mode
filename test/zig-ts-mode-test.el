;;; zig-ts-mode-test.el --- Zig mode regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'treesit)
(require 'imenu)

(when-let* ((directory (getenv "ZIG_TS_GRAMMAR_DIR")))
  (add-to-list 'treesit-extra-load-path directory))

;; Load the source so an older local .elc cannot hide regressions.
(load (expand-file-name "../zig-ts-mode.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro zig-ts-test--with-buffer (text &rest body)
  "Run BODY in a Zig buffer containing TEXT, with point at its beginning."
  (declare (indent 1))
  `(with-temp-buffer
     (unless (treesit-language-available-p 'zig)
       (error "Install the Zig grammar or set ZIG_TS_GRAMMAR_DIR"))
     (insert ,text)
     (zig-ts-mode)
     (goto-char (point-min))
     ,@body))

(ert-deftest zig-ts-test-newline-does-not-continue-string-contents ()
  (dolist (text '("const url = \"https://example.com\";"
                  "const s = \"\\\\hello\";"))
    (zig-ts-test--with-buffer text
      (goto-char (point-max))
      (zig-ts--comment-indent-new-line)
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     (concat text "\n"))))))

(ert-deftest zig-ts-test-newline-before-comment ()
  (zig-ts-test--with-buffer "const x = 1; // comment"
    (search-forward ";")
    (zig-ts--comment-indent-new-line)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "const x = 1;\n// comment"))))

(ert-deftest zig-ts-test-comment-continuation ()
  (dolist (prefix '("// " "/// " "//! "))
    (zig-ts-test--with-buffer (concat "    " prefix "hello world")
      (search-forward "hello")
      (zig-ts--comment-indent-new-line)
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     (concat "    " prefix "hello\n    " prefix "world"))))))

(ert-deftest zig-ts-test-trailing-comment-column ()
  (zig-ts-test--with-buffer "\tconst x = 1; // hello"
    (search-forward "//")
    (let ((column (- (current-column) 2)))
      (goto-char (point-max))
      (zig-ts--comment-indent-new-line)
      (beginning-of-line)
      (skip-chars-forward " ")
      (should (= (current-column) column))
      (should (looking-at "// ")))))

(ert-deftest zig-ts-test-multiline-string-continuation ()
  (dolist (soft '(nil t))
    (zig-ts-test--with-buffer "const s =\n    \\\\hello world\n;\n"
      (search-forward "hello")
      (zig-ts--comment-indent-new-line soft)
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "const s =\n    \\\\hello\n    \\\\world\n;\n")))))

(ert-deftest zig-ts-test-multiline-string-syntax ()
  (zig-ts-test--with-buffer "const s =\n    \\\\it's \"text\" // literal \\\n    \\\\another line\n;\nfn main() void {}\n"
    (search-forward "literal")
    (should (nth 3 (syntax-ppss)))
    (should-not (nth 4 (syntax-ppss)))
    (search-forward "fn main")
    (should-not (nth 3 (syntax-ppss)))
    (should-not (nth 4 (syntax-ppss)))))

(ert-deftest zig-ts-test-multiline-string-edit ()
  (zig-ts-test--with-buffer "const s =\n    \\\\plain\n;\nfn main() void {}\n"
    (syntax-propertize (point-max))
    (search-forward "plain")
    (insert " it's \"quoted\" \\")
    (search-forward "fn main")
    (should-not (nth 3 (syntax-ppss)))
    (goto-char (point-min))
    (search-forward "\\\\")
    (replace-match "//" t t)
    (search-forward "quoted")
    (should (nth 4 (syntax-ppss)))
    (should-not (nth 3 (syntax-ppss)))
    (search-forward "fn main")
    (should-not (nth 3 (syntax-ppss)))))

(ert-deftest zig-ts-test-ordinary-string-and-character-syntax ()
  (zig-ts-test--with-buffer "const s = \"\\\\hello\"; const c = 'x';\n"
    (search-forward "hello")
    (should (nth 3 (syntax-ppss)))
    (search-forward "'x")
    (should (nth 3 (syntax-ppss)))
    (goto-char (point-max))
    (should-not (nth 3 (syntax-ppss)))))

(ert-deftest zig-ts-test-indent-parameters-and-opaque ()
  (zig-ts-test--with-buffer "const O = opaque {\nfn foo(\na: u32,\nb: u32,\n) void {\nconst x = 1;\n}\n};\n"
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "const O = opaque {\n    fn foo(\n        a: u32,\n        b: u32,\n    ) void {\n        const x = 1;\n    }\n};\n"))
    (should-not indent-tabs-mode)
    (let ((once (buffer-string)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) once)))))

(ert-deftest zig-ts-test-doc-comments ()
  (dolist (prefix '("///" "//!"))
    (zig-ts-test--with-buffer (concat prefix " documentation\n")
      (font-lock-ensure)
      (search-forward "documentation")
      (should (eq (get-text-property (1- (point)) 'face) 'font-lock-doc-face))
      (backward-char)
      (should (zig-ts--fill-paragraph)))))

(ert-deftest zig-ts-test-doc-comment-boundaries ()
  (dolist (case '(("// ordinary" . nil)
                  ("//// ordinary" . nil)
                  ("///// ordinary" . nil)
                  ("///" . t)
                  ("///doc" . t)
                  ("//!" . t)
                  ("//!doc" . t)))
    (zig-ts-test--with-buffer (concat "    " (car case) "\n")
      (font-lock-ensure)
      (search-forward (car case))
      (backward-char)
      (should (eq (get-text-property (point) 'face)
                  (if (cdr case) 'font-lock-doc-face 'font-lock-comment-face)))
      (should (eq (zig-ts--fill-paragraph) (cdr case))))))

(ert-deftest zig-ts-test-imenu-test-names ()
  (zig-ts-test--with-buffer "test \"named\" {}\ntest { const x = 1; }\n"
    (let* ((index (funcall imenu-create-index-function))
           (names (mapcar #'car (cdr (assoc "Test" index)))))
      (should (equal names '("\"named\"" "test at line 2"))))))

(ert-deftest zig-ts-test-file-commands-require-visited-file ()
  (with-temp-buffer
    (dolist (command '(zig-ts-build-exe zig-ts-build-lib zig-ts-build-obj
                      zig-ts-run zig-ts-test))
      (should-error (funcall command) :type 'user-error))))

(ert-deftest zig-ts-test-custom-executable ()
  (let ((zig-ts-zig-bin "/tmp/zig tools/zig"))
    (zig-ts-test--with-buffer ""
      (should (equal compile-command
                     (concat (shell-quote-argument zig-ts-zig-bin) " build")))
      (setq buffer-file-name "/tmp/source file.zig")
      (let (command)
        (cl-letf (((symbol-function 'save-some-buffers) #'ignore)
                  ((symbol-function 'compilation-start)
                   (lambda (cmd &rest _) (setq command cmd))))
          (zig-ts-run))
        (should (equal command
                       (mapconcat #'shell-quote-argument
                                  (list zig-ts-zig-bin "run" buffer-file-name "-O" "Debug")
                                  " ")))))))

;;; zig-ts-mode-test.el ends here
