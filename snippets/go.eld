go-mode go-ts-mode

;; Package and imports
(pkg "package " q)
(imp "import \"" q "\"")
(impn "import (" n> q n ")")

;; Functions
(fn "func " p "(" p ") " p " {" n> q n "}")
(fne "func " p "(" p ") (" p ", error) {" n> q n "}")
(mn "func main() {" n> q n "}")
(in "func init() {" n> q n "}")
(mth "func (" p " *" p ") " p "(" p ") " p " {" n> q n "}")

;; Types
(stt "type " p " struct {" n> q n "}")
(inf "type " p " interface {" n> q n "}")
(cnt "const " p " = " q)
(cnst "const (" n> p " = " q n ")")
(vr "var " p " " q)
(mp "map[" p "]" q)

;; Control flow
(if "if " p " {" n> q n "}")
(ifel "if " p " {" n> p n "} else {" n> q n "}")
(elif "if " p " {" n> p n "} else if " p " {" n> p n "} else {" n> q n "}")
(for "for " p " {" n> q n "}")
(rng "for " p ", " p " := range " p " {" n> q n "}")
(sw "switch " p " {" n> "case " p ":" n> q n "}")
(sel "select {" n> "case " p ":" n> q n "}")

;; Error handling
(err "if err != nil {" n> "return " q "err" n "}")
(errf "if err != nil {" n> "return fmt.Errorf(\"" p ": %w\", err)" n "}")
(ifen "if err != nil {" n> q n "}")

;; Printing and logging
(pf "fmt.Printf(\"" p "\\n\", " q ")")
(pln "fmt.Println(" q ")")
(spf "fmt.Sprintf(\"" p "\", " q ")")
(errs "fmt.Errorf(\"" p ": %w\", " q ")")
(lf "log.Printf(\"" p "\\n\", " q ")")
(lln "log.Println(" q ")")

;; Testing
(tst "func Test" p "(t *testing.T) {" n> q n "}")
(tstb "func Benchmark" p "(b *testing.B) {" n> "for i := 0; i < b.N; i++ {" n> q n "}" n "}")
(tstt "t.Run(\"" p "\", func(t *testing.T) {" n> q n "})")

;; Goroutines and channels
(go "go func() {" n> q n "}()")
(ch "make(chan " p ", " q ")")
(def ":= " q)
