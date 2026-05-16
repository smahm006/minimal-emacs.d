python-mode python-ts-mode python-base-mode

;; Script boilerplate
(script "#!/usr/bin/env python3" n n "def main():" n> "pass" n n n "if __name__ == '__main__':" n> "main()")
(ifmain "if __name__ == '__main__':" n> q)

;; Imports
(imp "import " q)
(fimp "from " p " import " q)

;; Functions and classes
(fn "def " p "(" p "):" n> q)
(fna "async def " p "(" p "):" n> q)
(cls "class " p ":" n> "def __init__(self" p "):" n> q)
(clsi "class " p "(" p "):" n> "def __init__(self" p "):" n> q)
(init "def __init__(self" p "):" n> q)
(mth "def " p "(self" p "):" n> q)
(smth "@staticmethod" n "def " p "(" p "):" n> q)
(cmth "@classmethod" n "def " p "(cls" p "):" n> q)
(prop "@property" n "def " p "(self):" n> q)

;; Dataclass
(dc "from dataclasses import dataclass" n n "@dataclass" n "class " p ":" n> p ": " q)

;; Control flow
(if "if " p ":" n> q)
(ifel "if " p ":" n> p n "else:" n> q)
(elif "if " p ":" n> p n "elif " p ":" n> p n "else:" n> q)
(for "for " p " in " p ":" n> q)
(while "while " p ":" n> q)
(with "with " p " as " p ":" n> q)
(sw "match " p ":" n> "case " p ":" n> q)

;; Error handling
(try "try:" n> p n "except " p " as e:" n> q)
(tryel "try:" n> p n "except " p " as e:" n> p n "else:" n> q)
(tryf "try:" n> p n "except " p " as e:" n> p n "finally:" n> q)

;; Comprehensions
(list "[" p " for " p " in " p "]")
(dict "{" p ": " p " for " p " in " p "}")
(gen "(" p " for " p " in " p ")")

;; Docstrings
(doc "\"\"\"" n p n "\"\"\"" n q)
(docf "\"\"\"" p n n "Args:" n> p ": " p n n "Returns:" n> q n "\"\"\"")

;; Printing and debugging
(p "print(" q ")")
(pf "print(f\"" p "\")")
(dbg "import pdb; pdb.set_trace()")
(pp "from pprint import pprint" n "pprint(" q ")")

;; Type annotations
(tp "def " p "(" p ": " p ") -> " p ":" n> q)
(ret "return " q)

;; Testing
(tst "def test_" p "():" n> q)
(tstc "class Test" p "(unittest.TestCase):" n> "def test_" p "(self):" n> q)
(assert "assert " p ", \"" q "\"")

;; Logging
(log "import logging" n "logger = logging.getLogger(__name__)")
(logf "logger." p "(f\"" q "\")")

;; Lambda
(lam "lambda " p ": " q)
