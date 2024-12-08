(in-package :pwi/aoc24)

(require :uiop)

(defun read-input (filename)
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
  (multiple-value-bind (rows cols) (calculate-array-dimensions input)
    (let ((array (make-array (list rows cols))))
      (loop for row from 0
            for str in input
            do (loop for col from 0
                     for char across str
                     do (setf (aref array row col) char)))
      array)))

(defun check-for-match (start-row start-col direction array string)
  (if (zerop (length string))
      t
      (let ((in-bounds (within-bounds (cons start-row start-col) array)))
        (and in-bounds                       
           (char-equal (aref array start-row start-col) (char string 0))
           (check-for-match (+ start-row (car direction))
                            (+ start-col (cdr direction))
                            direction
                            array
                            (subseq string 1))))))
  
(defun find-word-at-position (startpos direction array string)
  (let ((found 0))
    (when (char-equal (aref array (first startpos) (second startpos)) (char string 0))
      (when (check-for-match (+ (first startpos) (car direction))
                             (+ (second startpos) (cdr direction))
                             direction
                             array
                             (subseq string 1))
        (setf found 1)))
    found))

(defun count-all-occurrences (array search-string)
  (let ((size (array-dimension array 0)))
    (loop for row from 0 below size sum
          (loop for col from 0 below size sum
                (loop for (nil dir) on *directions* by #'cddr sum
                      (find-word-at-position
                        (list col row)
                        dir
                        array
                        search-string))))))

(defun solve (filename search-string)
  (let* ((input (read-input filename))
         (array (create-2d-array-from input))
         (result (count-all-occurrences array search-string)))
    (format t "Found ~a occurrences of ~a" result search-string)))
