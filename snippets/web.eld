js-mode js-ts-mode jsx-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode svelte-ts-mode

;; Imports
(imp "import " p " from '" q "';")
(impn "import { " p " } from '" q "';")
(impa "import * as " p " from '" q "';")
(req "const " p " = require('" q "');")

;; Functions
(fn "function " p "(" p ") {" n> q n "}")
(fna "async function " p "(" p ") {" n> q n "}")
(arr "const " p " = (" p ") => {" n> q n "};")
(arrr "const " p " = (" p ") => " q ";")
(arra "const " p " = async (" p ") => {" n> q n "};")
(lam "(" p ") => " q)

;; Control flow
(if "if (" p ") {" n> q n "}")
(ifel "if (" p ") {" n> p n "} else {" n> q n "}")
(for "for (let " p " = 0; " p " < " p "; " p "++) {" n> q n "}")
(forof "for (const " p " of " p ") {" n> q n "}")
(forin "for (const " p " in " p ") {" n> q n "}")
(while "while (" p ") {" n> q n "}")
(sw "switch (" p ") {" n> "case " p ":" n> q n> "break;" n "}")

;; Classes
(cls "class " p " {" n> "constructor(" p ") {" n> q n "}" n "}")
(clsi "class " p " extends " p " {" n> "constructor(" p ") {" n> "super(" p ");" n> q n "}" n "}")

;; Error handling
(try "try {" n> p n "} catch (" p ") {" n> q n "}")
(tryf "try {" n> p n "} catch (" p ") {" n> p n "} finally {" n> q n "}")
(prom "new Promise((resolve, reject) => {" n> q n "})")

;; Destructuring
(dest "const { " p " } = " q ";")
(destar "const [" p "] = " q ";")
(spr "const " p " = { ..." p ", " q " };")

;; Console
(cl "console.log(" q ");")
(ce "console.error(" q ");")
(cw "console.warn(" q ");")
(ct "console.table(" q ");")

;; Common patterns
(tof "typeof " p " === '" q "'")
(nul p " ?? " q)
(opt p "?." q)
(ret "return " q ";")
