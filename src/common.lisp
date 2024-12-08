(in-package :pwi/aoc24)

(defun calculate-array-dimensions (input)
  "Returns the size dimensions of the array. Also works for an array of strings."
  (if (arrayp input)
      (values (array-dimension input 0)
              (array-dimension input 1))
      (values (length input)
              (length (first input)))))

(defconstant +directions+
  (list :right (cons 0 1)
        :left (cons 0 -1)
        :up (cons -1 0)
        :down (cons 1 0)
        :up-right (cons -1 1)
        :down-right (cons 1 1)
        :up-left (cons -1 -1)
        :down-left (cons 1 -1)))

