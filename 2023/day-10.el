(progn
  (switch-to-buffer-other-window (get-file-buffer "input/day-10-input-readable.txt"))
  (goto-char 0)
  (search-forward "S")
  (string (following-char)))
