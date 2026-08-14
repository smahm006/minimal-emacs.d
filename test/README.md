# Configuration smoke tests

These checks run against the real configuration in an isolated named Emacs
daemon. They create temporary files and a temporary project, then remove them
after each run. Do not use the working daemon: it may contain user buffers.

## Start an isolated daemon

From `/home/smahm/.config/emacs`, start a daemon with a distinct server name:

```sh
time emacs --daemon=emacs-smoke --debug-init
```

The command loads the normal Emacs configuration, including `init.el`. Keep the
server name unique if another smoke test is already running.

## Run the smoke checks

```sh
emacsclient -s emacs-smoke \
  -e '(progn (load "/home/smahm/.config/emacs/test/config-smoke.el" nil nil t) (me/config-smoke-run t))'
```

The report appears in the `*config-smoke*` buffer and the returned plist can be
captured by callers that need machine-readable results. It records each
representative file's opening time, selected major mode, active minor modes,
Eglot state, newly started server processes, errors, and messages. It also
exercises Dired refresh and reports whether the planned path-copy command is
available.

For a direct first/subsequent Python file-open measurement, run the same
expression twice in the daemon and compare the `python` elapsed values. The
daemon startup time is the `time` output from the start command above.

## Stop the daemon

```sh
emacsclient -s emacs-smoke -e '(kill-emacs)'
```

## Baseline tool inventory

Record formatter and run-command prerequisites before changing configuration:

```sh
for tool in ruff ty goimports gofumpt shfmt google-java-format rustfmt \
  clang-format zig prettier yamlfmt jq; do
  if command -v "$tool" >/dev/null 2>&1; then
    printf '%-22s %s\n' "$tool" "$(command -v "$tool")"
  else
    printf '%-22s %s\n' "$tool" "missing"
  fi
done
```

The baseline should preserve the `treesit-query-error` signature and the
minor-mode state reported for Python before any simplification ticket changes
the configuration.
