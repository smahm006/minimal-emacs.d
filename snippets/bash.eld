sh-mode sh-ts-mode bash-ts-mode
(shbang "#!/usr/bin/env bash")
(if "if " p n "then" n> p n "fi" > q)
(else "if " p n "then" n> p n "else" n> p n "fi" > q)
(elif "if " p n "then" n> p n "elif " p n "then" n> p n "else" n> p n "fi" > q)
(varempty "[ -z \"${" p "}\" ]")
(varexists "[ -n \"${" p "}\" ]")
(direxists "[ -d \"" p "\" ]")
