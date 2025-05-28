fundamental-mode
(today (format-time-string "%Y-%m-%d"))
(NOW (format-time-string "%Y-%m-%d %a %H:%M"))
(yesterday (format-time-string "%Y-%m-%d" (time-subtract nil (* 24 60 60))))
(tomorrow (format-time-string "%Y-%m-%d" (time-add nil (* 24 60 60))))
