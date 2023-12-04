;; Adapted from https://gist.github.com/amno1/08f4d98b425b44bb342acbfbe183116a

(defun save-input-for-day (arg day)
  "Save the input of the year 2023 and DAY."
  (interactive "P\nnAdvent of Code 2023 Day: ")
  (let* ((year 2023)
         (url (format "https://adventofcode.com/2023/day/%s/input" day))
         (input (expand-file-name (format "input/day-%02d-input.txt" day)
                                  default-directory))
         (cookie (with-temp-buffer
                   (insert-file-contents (expand-file-name "session_id" default-directory))
                   (buffer-substring-no-properties (point-min) (point-max))))
         (url-request-extra-headers (list (cons "Cookie" (concat "session=" cookie)))))
    (url-copy-file url input)))
