sh-mode sh-ts-mode bash-ts-mode

;; Shebang
(shbang "#!/usr/bin/env bash" n q)

;; Control flow
(if "if " p "; then" n> q n "fi")
(ifel "if " p "; then" n> p n "else" n> q n "fi")
(elif "if " p "; then" n> p n "elif " p "; then" n> p n "else" n> q n "fi")
(for "for " p " in " p "; do" n> q n "done")
(while "while " p "; do" n> q n "done")
(case "case " p " in" n> p ")" n> q n ";;" n "esac")

;; Functions
(fn "function " p "() {" n> q n "}")

;; Variable checks
(varempty "[ -z \"${" p "}\" ]")
(varexists "[ -n \"${" p "}\" ]")
(direxists "[ -d \"" p "\" ]")
(fileexists "[ -f \"" p "\" ]")

;; Common patterns
(readonly "readonly " p "=" q)
(export "export " p "=" q)
(src "source " q)
(trap "trap '" p "' " q)
(err "echo \"Error: " p "\" >&2" n "exit 1")
