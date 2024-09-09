set dotenv-load

emacs := env("EMACS", "emacs")
test-file := env("TEST_FILE", "./test/main.zig")

eval:
    {{emacs}} -Q --debug-init -L . --eval "(require 'zig-ts-mode)" {{test-file}}

lint:
    ./makem.sh lint -E {{emacs}}

