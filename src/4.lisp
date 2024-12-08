(in-package :pwi/aoc24)

(require :uiop)

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
