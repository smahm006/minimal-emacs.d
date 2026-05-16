c-mode c-ts-mode

;; Headers and includes
(inc "#include <" q ">")
(incs "#include \"" q "\"")
(once "#ifndef " p "_H" n "#define " p "_H" n n q n n "#endif /* " p "_H */")

;; Functions
(fn p " " p "(" p ") {" n> q n "}")
(mn "int main(int argc, char *argv[]) {" n> q n n> "return 0;" n "}")
(proto p " " p "(" p ");")

;; Control flow
(if "if (" p ") {" n> q n "}")
(ifel "if (" p ") {" n> p n "} else {" n> q n "}")
(elif "if (" p ") {" n> p n "} else if (" p ") {" n> p n "} else {" n> q n "}")
(for "for (" p "; " p "; " p ") {" n> q n "}")
(while "while (" p ") {" n> q n "}")
(do "do {" n> q n "} while (" p ");")
(sw "switch (" p ") {" n> "case " p ":" n> q n> "break;" n "}")

;; Types and structs
(stt "typedef struct {" n> q n "} " p ";")
(en "typedef enum {" n> q n "} " p ";")
(td "typedef " p " " q ";")

;; Memory
(mal p " = malloc(sizeof(" p "));" n "if (" p " == NULL) {" n> q n "}")
(free "free(" p ");" n p " = NULL;")

;; Printing
(pf "printf(\"" p "\\n\", " q ");")
(pln "printf(\"" p "\\n\");")
(ef "fprintf(stderr, \"Error: " p "\\n\", " q ");")

;; Error handling
(err "if (" p " < 0) {" n> "perror(\"" p "\");" n> "exit(EXIT_FAILURE);" n "}")
