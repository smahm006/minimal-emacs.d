zig-mode zig-ts-mode

;; Imports
(imp "const " p " = @import(\"" q "\");")
(impstd "const std = @import(\"std\");")

;; Functions
(fn "fn " p "(" p ") " p " {" n> q n "}")
(fnp "pub fn " p "(" p ") " p " {" n> q n "}")
(mn "pub fn main() !void {" n> q n "}")
(tst "test \"" p "\" {" n> q n "}")

;; Types
(stt "const " p " = struct {" n> q n "};")
(en "const " p " = enum {" n> q n "};")
(un "const " p " = union {" n> q n "};")
(err "const " p " = error{" n> q n "};")

;; Control flow
(if "if (" p ") {" n> q n "}")
(ifel "if (" p ") {" n> p n "} else {" n> q n "}")
(for "for (" p ") |" p "| {" n> q n "}")
(while "while (" p ") {" n> q n "}")
(sw "switch (" p ") {" n> p " => " q "," n "}")

;; Error handling
(try "try " q ";")
(catch p " catch |err| {" n> q n "}")
(erru "return error." q ";")

;; Memory
(alloc "var " p " = try allocator.alloc(" p ", " q ");")
(free "allocator.free(" q ");")

;; Printing
(pln "std.debug.print(\"" p "\\n\", .{" q "});")
(pf "std.debug.print(\"" p "\", .{" q "});")
