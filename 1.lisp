(defun read-two-columns-from-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
          while line
          for (col1 col2) = (with-input-from-string (str line)
                              (list (read str nil nil)
                                    (read str nil nil)))
          collect col1 into column1
          collect col2 into column2
          finally (return (values column1 column2)))))

(defun sort-columns (col1 col2)
  (setf col1 (sort col1 #'<))
  (setf col2 (sort col2 #'<))
  (values col1 col2))

(defun distance (col1 col2)
  (reduce #'+
          (loop for num1 in col1
                for num2 in col2
                collect (if (> num1 num2) (- num1 num2) (- num2 num1)))))

(defun process-inputs (filename)
  (multiple-value-bind (sorted-column1 sorted-column2)
      (apply #'sort-columns
             (multiple-value-list (read-two-columns-from-file filename)))
    ; Do something with the sorted columns here, such as get the distance.
    (distance sorted-column1 sorted-column2)))
