(require :uiop)

(defparameter *directions*
  ; Re-use from day 4
  (list :right (cons 0 1)
        :left (cons 0 -1)
        :up (cons -1 0)
        :down (cons 1 0)
        :up-right (cons -1 1)
        :down-right (cons 1 1)
        :up-left (cons -1 -1)
        :down-left (cons 1 -1)))

(defun read-input (filename)
  ; Re-use from day 4
  ;; Read filename as a list of strings; remove all newlines.
  ; Note to self: :separator takes a sequence, so requires #\Newline to be in a
  ; list instead of passed directly.
  ; Another note to self: splitting a string on newline characters creates an
  ; empty string. Use remove-if #'uiop:emptyp to drop the empty string from the
  ; result.
  (remove-if #'uiop:emptyp
             (uiop:split-string
              (uiop:read-file-string filename)
              :separator '(#\Newline #\Return))))

(defun calculate-array-dimensions (input)
  "Returns the size dimensions of the array. Also works for an array of strings."
  (if (arrayp input)
      (values (array-dimension input 0)
              (array-dimension input 1))
      (values (length input)
              (length (first input)))))

(defun create-2d-array-from (input)
  ; Re-use from day 4
  (multiple-value-bind (rows cols) (calculate-array-dimensions input)
    (let ((array (make-array (list rows cols))))
      (loop for row from 0
            for str in input
            do (loop for col from 0
                     for char across str
                     do (setf (aref array row col) char)))
      array)))

(defun within-bounds (coordinates array)
  ; Adapted from day 4
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

(defun find-guard (array)
  "Returns two values:

   - a cons cell that represents a position, (row . col), of a guard in the array,
   - the direction of the guard, indicated by an arrow: ^ > v <

  If no guard is found, returns `nil nil`."
  (multiple-value-bind (rows cols) (calculate-array-dimensions array)
    (loop named outer for row below rows
          do (loop named inner for col below cols
                   when (or
                         (char= (aref array row col) #\^)
                         (char= (aref array row col) #\>)
                         (char= (aref array row col) #\v)
                         (char= (aref array row col) #\<))
                     do (return-from outer (values (cons row col) (aref array row col))))
          finally (return-from outer (values nil nil)))))

(defun get-direction (face)
  "Return the direction a guard is facing (^>v<) as a keyword:
  :up, :right, :down, :left, respectively; nil for invalid face."
  (cond
    ((char= face #\^) :up)
    ((char= face #\>) :right)
    ((char= face #\v) :down)
    ((char= face #\<) :left)))

(defun next-position (position direction)
  (let* ((offset (getf *directions* direction))
         (x (+ (car position) (car offset)))
         (y (+ (cdr position) (cdr offset))))
    (cons x y)))

(defun is-occupied-p (position map)
  (and (within-bounds position map)
       (char= #\# (aref map (car position) (cdr position)))))

(defun turn (direction)
  (cond ((eq direction :up) :right)
        ((eq direction :right) :down)
        ((eq direction :down) :left)
        ((eq direction :left) :up)))

(defun walk-guard (array)
  "Walk the guard around the map until it finds an exit.

  Returns the number of moves made ('steps taken') by the guard."
  (multiple-value-bind (start-position start-direction) (find-guard array)
    (assert (not (or (null start-position)
                     (null start-direction)))
            nil
            "Couldn't find a guard on the map!")
    (let ((position start-position)
          (direction (get-direction start-direction))
          (visited (make-hash-table :test #'equal)))
      (setf (gethash position visited) t)
      (loop
        (let ((next-pos (next-position position direction)))
          (unless (within-bounds next-pos array)
            (return (hash-table-count visited)))
          (if (is-occupied-p next-pos array)
              (setf direction (turn direction))
              (progn
                (setf position next-pos)
                (setf (gethash position visited) t))))))))

(defun solve (filename)
  "Solve the guard maze problem for the given input file.
   Returns the number of steps taken by the guard to reach an exit."
  (let ((maze (create-2d-array-from (read-input filename))))
    (walk-guard maze)))
