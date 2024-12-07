(in-package :pwi/aoc24)

(require :cl-ppcre)

(defvar *regex-pattern-1* "mul\\(\\d{1,3},\\d{1,3}\\)")

(defun read-input (filename)
  (with-open-file (in filename)
    (let ((contents (make-string (file-length in))))
      (read-sequence contents in)
      contents)))

(defun respect-do-not-instruction (string)
  (cl-ppcre:regex-replace-all "don't\\(\\).?do\\(\\)" string "don't()do()"))

(defun find-muls (filename)
  (let* ((input-string (read-input filename))
         (clean-string (respect-do-not-instruction input-string)))
    (cl-ppcre:all-matches-as-strings "mul\\(\\d{1,3},\\d{1,3}\\)" clean-string)))

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
