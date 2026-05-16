org-mode

;; Source blocks
(src "#+begin_src " p n q n "#+end_src")
(src-py "#+begin_src python" n q n "#+end_src")
(src-go "#+begin_src go" n q n "#+end_src")
(src-sh "#+begin_src bash" n q n "#+end_src")
(src-el "#+begin_src emacs-lisp" n q n "#+end_src")
(src-rs "#+begin_src rust" n q n "#+end_src")
(src-c "#+begin_src C" n q n "#+end_src")
(src-js "#+begin_src js" n q n "#+end_src")
(src-ts "#+begin_src typescript" n q n "#+end_src")
(src-yaml "#+begin_src yaml" n q n "#+end_src")
(src-json "#+begin_src json" n q n "#+end_src")

;; Blocks
(quote "#+begin_quote" n q n "#+end_quote")
(ex "#+begin_example" n q n "#+end_example")
(note "#+begin_note" n q n "#+end_note")
(verse "#+begin_verse" n q n "#+end_verse")

;; Document headers
(title "#+title: " q)
(author "#+author: " q)
(date "#+date: " (format-time-string "%Y-%m-%d") q)
(opts "#+options: " q)
(prop "#+property: " q)

;; Links and images
(link "[[" p "][" q "]]")
(img "[[file:" p "][" q "]]")

;; Tables
(tbl "| " p " | " q " |" n "|-")

;; Headings with todo
(todo "* TODO " q)
(doing "* DOING " q)
(done "* DONE " q)
