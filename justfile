set dotenv-load

emacs := env("EMACS", "emacs")
test-file := env("TEST_FILE", "./test/main.zig")

eval:
    {{emacs}} -Q --debug-init -L . --eval "(require 'zig-ts-mode)" {{test-file}}

lint:
    {{emacs}} -Q --batch -l bytecomp --eval '(let* ((byte-compile-error-on-warn t) (output (make-temp-file "zig-ts-mode-" nil ".elc")) (byte-compile-dest-file-function (lambda (_) output))) (unwind-protect (unless (byte-compile-file "zig-ts-mode.el") (kill-emacs 1)) (delete-file output)))'

test:
    {{emacs}} -Q --batch -L . -l test/zig-ts-mode-test.el -f ert-run-tests-batch-and-exit
