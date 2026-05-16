fundamental-mode

;; Dates and times
(today (format-time-string "%Y-%m-%d"))
(now (format-time-string "%Y-%m-%d %a %H:%M"))
(yesterday (format-time-string "%Y-%m-%d" (time-subtract nil (* 24 60 60))))
(tomorrow (format-time-string "%Y-%m-%d" (time-add nil (* 24 60 60))))

;; Common annotations
(todo "TODO: " q)
(fixme "FIXME: " q)
(hack "HACK: " q)
(note "NOTE: " q)
(xxx "XXX: " q)

;; Lorem ipsum for placeholder text
(lorem "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
