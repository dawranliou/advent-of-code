;; Adapted from https://gist.github.com/amno1/08f4d98b425b44bb342acbfbe183116a

(defun aoc-save-input-for-day (arg day)
  "Save the input of the year 2024 and DAY."
  (interactive "P\nnAdvent of Code 2024 Day: ")
  (let* ((year "2024")
         (url (format "https://adventofcode.com/%s/day/%s/input" year day))
         (input (expand-file-name (format "day%02d.input" day)
                                  default-directory))
         (cookie (with-temp-buffer
                   (insert-file-contents (expand-file-name "session_id" default-directory))
                   (buffer-substring-no-properties (point-min) (point-max))))
         (url-request-extra-headers (list (cons "Cookie" (concat "session=" cookie)))))
    (url-copy-file url input)))
