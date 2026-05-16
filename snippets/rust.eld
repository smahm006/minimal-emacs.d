rust-mode rust-ts-mode

;; Module and imports
(use "use " q ";")
(usep "use " p "::{" q "};")
(mod "mod " p " {" n> q n "}")
(pub "pub " q)

;; Functions
(fn "fn " p "(" p ") {" n> q n "}")
(fnr "fn " p "(" p ") -> " p " {" n> q n "}")
(fna "async fn " p "(" p ") {" n> q n "}")
(fnar "async fn " p "(" p ") -> " p " {" n> q n "}")
(mn "fn main() {" n> q n "}")

;; Types
(stt "struct " p " {" n> q n "}")
(en "enum " p " {" n> q n "}")
(tr "trait " p " {" n> q n "}")
(imp "impl " p " {" n> q n "}")
(impt "impl " p " for " p " {" n> q n "}")
(tyal "type " p " = " q ";")

;; Control flow
(if "if " p " {" n> q n "}")
(ifel "if " p " {" n> p n "} else {" n> q n "}")
(for "for " p " in " p " {" n> q n "}")
(while "while " p " {" n> q n "}")
(loop "loop {" n> q n "}")
(sw "match " p " {" n> p " => " q "," n "}")
(if_let "if let " p " = " p " {" n> q n "}")
(while_let "while let " p " = " p " {" n> q n "}")

;; Error handling
(err "return Err(" q ".into());")
(ok "return Ok(" q ");")
(res "Result<" p ", " q ">")
(opt "Option<" q ">")
(try "let " p " = " p "?;")
(unwrap p ".unwrap_or_else(|e| {" n> q n "})")

;; Printing
(pln "println!(\"" p "\", " q ");")
(pf "print!(\"" p "\", " q ");")
(epln "eprintln!(\"" p "\", " q ");")
(dbg "dbg!(" q ");")

;; Testing
(tst "#[test]" n "fn test_" p "() {" n> q n "}")
(tstm "#[cfg(test)]" n "mod tests {" n> "use super::*;" n> q n "}")

;; Common derives
(der "#[derive(" q ")]")
(derc "#[derive(Debug, Clone)]")
(ders "#[derive(Debug, Clone, Serialize, Deserialize)]")

;; Closures
(cl "|" p "| {" n> q n "}")
(clr "|" p "| " q)

;; Lifetimes
(lt "<'" q ">")
