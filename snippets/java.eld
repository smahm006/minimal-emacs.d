java-mode java-ts-mode

;; Boilerplate
(cls "public class " p " {" n> q n "}")
(clsi "public class " p " extends " p " {" n> q n "}")
(clsim "public class " p " implements " p " {" n> q n "}")
(inf "public interface " p " {" n> q n "}")
(en "public enum " p " {" n> q n "}")
(rec "public record " p "(" p ") {" n> q n "}")

;; Functions
(fn "public " p " " p "(" p ") {" n> q n "}")
(fnp "private " p " " p "(" p ") {" n> q n "}")
(main "public static void main(String[] args) {" n> q n "}")
(lam "(" p ") -> " q)
(lamb "(" p ") -> {" n> q n "}")

;; Control flow
(if "if (" p ") {" n> q n "}")
(ifel "if (" p ") {" n> p n "} else {" n> q n "}")
(for "for (int " p " = 0; " p " < " p "; " p "++) {" n> q n "}")
(forr "for (" p " " p " : " p ") {" n> q n "}")
(while "while (" p ") {" n> q n "}")
(sw "switch (" p ") {" n> "case " p " -> " q ";" n "}")

;; Error handling
(try "try {" n> p n "} catch (" p " e) {" n> q n "}")
(tryf "try {" n> p n "} catch (" p " e) {" n> p n "} finally {" n> q n "}")

;; Common patterns
(pln "System.out.println(" q ");")
(pf "System.out.printf(\"" p "\", " q ");")
(err "System.err.println(\"Error: \" + " q ");")
(log "private static final Logger logger = LoggerFactory.getLogger(" p ".class);")

;; Testing
(tst "@Test" n "public void test" p "() {" n> q n "}")
(tstc "class Test" p " {" n> q n "}")

;; Annotations
(over "@Override" n q)
(dep "@Deprecated" n q)
