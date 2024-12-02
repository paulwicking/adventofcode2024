(defun check-secureness (line)
  (or (all-increasing line) (all-decreasing line)))

(defun all-increasing (lst)
  (loop for (a b) on lst
        while b
        always (and (< a b) (<= (- b a) 3))))

(defun all-decreasing (lst)
  (loop for (a b) on lst
        while b
        always (and (> a b) (<= (- a b) 3))))

(defun string-to-list (str)
  (with-input-from-string (s str)
    (loop for num = (read s nil nil)
          while num
          collect num)))

(defun read-strings (filename)
   ;; Reads `filename` line by line.
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
          while line
          count (check-secureness (string-to-list line)))))

