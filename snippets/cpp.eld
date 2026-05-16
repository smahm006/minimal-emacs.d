c++-mode c++-ts-mode

;; Headers and includes
(inc "#include <" q ">")
(incs "#include \"" q "\"")
(once "#pragma once" n q)

;; Namespace
(ns "namespace " p " {" n n> q n n "} // namespace " p)
(uns "using namespace " q ";")
(ua "using " p " = " q ";")

;; Functions
(fn p " " p "(" p ") {" n> q n "}")
(mn "int main(int argc, char* argv[]) {" n> q n n> "return 0;" n "}")
(lam "[" p "](" p ") {" n> q n "}")
(lamr "[" p "](" p ") -> " p " {" n> q n "}")

;; Classes
(cls "class " p " {" n "public:" n> q n "};")
(clsi "class " p " : public " p " {" n "public:" n> q n "};")
(stt "struct " p " {" n> q n "};")

;; Control flow
(if "if (" p ") {" n> q n "}")
(ifel "if (" p ") {" n> p n "} else {" n> q n "}")
(for "for (" p "; " p "; " p ") {" n> q n "}")
(forr "for (auto& " p " : " p ") {" n> q n "}")
(while "while (" p ") {" n> q n "}")
(sw "switch (" p ") {" n> "case " p ":" n> q n> "break;" n "}")

;; Templates
(tmpl "template<typename " p ">" n q)
(tmpls "template<typename " p ", typename " p ">" n q)

;; Smart pointers
(uniq "std::unique_ptr<" p "> " p " = std::make_unique<" p ">(" q ");")
(shr "std::shared_ptr<" p "> " p " = std::make_shared<" p ">(" q ");")

;; Printing
(cout "std::cout << " p " << std::endl;")
(cerr "std::cerr << \"Error: \" << " q " << std::endl;")

;; Error handling
(try "try {" n> p n "} catch (const std::exception& e) {" n> q n "}")
