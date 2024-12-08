(in-package :pwi/aoc24)

(defun calculate-array-dimensions (input)
  "Returns the size dimensions of the array. Also works for an array of strings."
  (if (arrayp input)
      (values (array-dimension input 0)
              (array-dimension input 1))
      (values (length input)
              (length (first input)))))
