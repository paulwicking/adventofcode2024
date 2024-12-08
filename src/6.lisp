(in-package :pwi/aoc24)

(require :uiop)
(require :alexandria)

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
  "Walk the guard around the map until it finds an exit or enters a loop.

   Returns two values:
   - :EXIT or :LOOP to indicate how the guard ended the patrol.
   - The number of unique positions visited by the guard."
  (multiple-value-bind (start-position start-direction) (find-guard array)
    (assert (not (or (null start-position)
                     (null start-direction)))
            nil
            "Couldn't find a guard on the map!")
    (let ((position start-position)
          (direction (get-direction start-direction))
          (visited-positions (make-hash-table :test #'equal))
          (visited-states (make-hash-table :test #'equal)))
      (setf (gethash position visited-positions) t)
      (setf (gethash (cons position direction) visited-states) t)
      (loop
        (let ((next-pos (next-position position direction)))
          (unless (within-bounds next-pos array)
            (return (values :exit (hash-table-count visited-positions))))
          (if (is-occupied-p next-pos array)
              (setf direction (turn direction))
              (progn
                (let ((pos-dir (cons next-pos direction)))
                  (when (gethash pos-dir visited-states)
                    (return (values :loop (hash-table-count visited-positions))))
                  (setf position next-pos)
                  (setf (gethash position visited-positions) t)
                  (setf (gethash pos-dir visited-states) t)))))))))

(defun occupied-position-p (position array)
  "TRUE if POSITION in ARRAY is occupied by guard or obstacle."
  (when (within-bounds position array)
    (let ((content (aref array (car position) (cdr position))))
      (find content "^>v<#"))))

(defun count-loop-creating-positions (map)
  "For each position in the MAP, unless already occupied by the guard or an
   obstacle, insert an obstacle, walk the guard, and see if the guard ends up in
   a loop or exits the map. Then restore the original content of the position
   and repeat. Returns the total count of positions that will cause the guard to
   loop forever."
  (let ((count 0)
        (test-map (alexandria:copy-array map)))
    (loop for i below (array-dimension test-map 0)
          do (loop for j below (array-dimension test-map 1)
                   do (let ((position (cons i j)))
                        (unless (occupied-position-p position test-map)
                          (let ((original (aref test-map i j)))
                            (setf (aref test-map i j) #\#)
                            (when (eq :loop (nth-value 0 (walk-guard test-map)))
                              (incf count))
                            (setf (aref test-map i j) original))))))
    count))

(defun solve (filename)
  "Solve the guard maze problem for the given input file.
   Returns the number of steps taken by the guard to reach an exit."
  (let ((maze (create-2d-array-from (read-input filename))))
    (nth-value 1 (walk-guard maze))))

(defun solve-again (filename)
  "Solve part two of the challenge."
  (let ((maze (create-2d-array-from (read-input filename))))
    (count-loop-creating-positions maze)))
