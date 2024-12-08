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

(defun within-bounds (coordinates array)
  "Check if the coordinates are within the boundaries of the array.

   COORDINATES is assumed to be a cons pair, (x . y).
   ARRAY is assumed to be a two-dimensional array with dimensions of equal size."
  (assert (= (array-dimension array 0) (array-dimension array 1)) nil "Non-square array!")
  (let ((gridsize (array-dimension array 0))
        (x (car coordinates))
        (y (cdr coordinates)))
    (and
     (>= x 0)
     (>= y 0)
     (< x gridsize)
     (< y gridsize))))

