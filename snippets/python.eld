python-mode python-ts-mode python-base-mode
(__contains__ "def __contains__(self, el):" n> p n> "pass")
(__enter__ "def __enter__(self):" n> p n> "return self")
(__eq__ "def __eq__(self, other):" n> "return self." p " == other." q)
(__exit__ "def __exit__(self, type, value, traceback):" n> p n> "pass")
(__getitem__ "def __len__(self):" n> p n> "pass")
(__repr__ "def __repr__(self):" n> q)
(__iter__ "def __iter__(self):" n> "return " q)
(__new__ "def __new__(mcs, name, bases, dict):" n> p n> "return type.__new__(mcs, name, bases, dict)")
(__setitem__ "__all__ = [" n> p n> "]")
(__str__ "def __str__(self):" n> q)
(parse_args "def parse_arguments():" n> "parser = argparse.ArgumentParser(description='" p "')" n> p n> "return parser.parse_args()")
(arg "parser.add_argument('-" p "', '--" p "'," n> p ")")
(arg_positional "parser.add_argument('" p "', " p ")")
(assert "assert " q)
(class "class " p":" n> "def __init__(self" p "):" n> q)
(classmethod "@classmethod" n> "def " p "(cls, " p "):" n> q)
(static "@staticmethod" n> "def " p "(" p "):" n> q)
(def "def " p "(self" p "):" n> q)
(def_decorator "def " p "(func):" n> p n> "def _" p "(*args, **kwargs):" n> p n> "ret = func(*args, **kwargs)" n> p n> "return ret" n n> "return _" q)
(doc "\"\"\"" p "\"\"\"")
(for "for " p " in " p ":" n> q)
(function_docstring "def " p "(" p "):" n> "\"\"\"" p "\"\"\"" n> q)
(if "if " p ":" n> q)
(ife "if " p ":" n> p n> "else:" n> q)
(ifmain "if __name__ == '__main__':" n> q)
(ig "# type: ignore" q)
(imp "import " q)
(fimp "from " p " import " q)
(init "def __init__(self" p "):" n> q)
(lambda "lambda " p ": " q)
(list "[" p " for " p " in " p "]")
(logger_name "logger = logging.getLogger(__name__)")
(logging "logger = logging.getLogger(\"" p "\")" n> "logger.setLevel(logging." p ")")
(p "print(\"" p "\")")
(pf "print(f\"" p "\")")
(return "return " q)
(script "#!/usr/bin/env python" n n> "def main():" n> "pass" n n> "if __name__ == '__main__':" n> "main()")
(super "super(" p ", self)." p "(" p ")")
(test_class "class Test" p "(" p "):" n> q)
(test_file "import pytest" n> "from " p " import *" n> p n> "if __name__ == '__main__':" n> "unittest.main()")
(trace "import pdb; pdb.set_trace()")
(try "try:" n> p n> "except " p ":" n> q)
(tryelse "try:" n> p n> "except " p ":" n> p n> "else:" n> q)
(while "while " p ":" n> q)
(with "with " p p ":" n> q)
