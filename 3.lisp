(require :cl-ppcre)

(defvar *regex-pattern-1* "mul\\(\\d{1,3},\\d{1,3}\\)")

(defun read-input (filename)
  (with-open-file (in filename)
    (let ((contents (make-string (file-length in))))
      (read-sequence contents in)
      contents)))

(defun find-muls (filename)
  (let ((input-string (read-input filename)))
    (cl-ppcre:all-matches-as-strings "mul\\(\\d{1,3},\\d{1,3}\\)" input-string)))

(defun clean-mul-pairs (mul-list)
  (mapcar (lambda (mul-str)
            (mapcar #'parse-integer 
                   (cl-ppcre:all-matches-as-strings "\\d+" mul-str)))
          mul-list))

(defun multiply-pairs (pairs)
  (mapcar (lambda (pair)
            (* (first pair) (second pair)))
          pairs))

(defun generate-solution (input-file)
  (reduce #'+
          (multiply-pairs
           (clean-mul-pairs
            (find-muls input-file)))))
