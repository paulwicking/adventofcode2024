(in-package :pwi/aoc24)

(require :uiop)

(defun read-rules-and-updates-from (filename)
  "Input is expected to include an empty line that signifies separation of content."
  (let ((all-lines (uiop:read-file-lines filename)))
    (loop for line in all-lines
          until (string= line "")
          collect (let ((parts (mapcar #'parse-integer
                                       (uiop:split-string line :separator "|"))))
                    (cons (first parts) (second parts))) into rules
          finally
             (return
               (values rules
                       (loop for line in (rest (member "" all-lines :test #'string=))
                             collect (mapcar #'parse-integer
                                             (uiop:split-string line :separator ","))))))))


(defun add-dependency (graph direction key value)
    (cond ((eq direction :successors)
            (let ((table (gethash direction graph)))
              (unless (gethash key table)
                (setf (gethash key table) nil))
              (setf (gethash key table)
                    (adjoin value (gethash key table)))))
          ((eq direction :predecessors)
           (let ((table (gethash direction graph)))
             (unless (gethash value table)
               (setf (gethash value table) nil))
             (setf (gethash value table)
                   (adjoin key (gethash value table))))))
  nil)

(defun create-dependency-graph (rules)
  (let ((graph (make-hash-table)))
    (setf (gethash :successors graph) (make-hash-table))
    (setf (gethash :predecessors graph) (make-hash-table))
    (loop for (predecessor . successor) in rules
          do (add-dependency graph :successors predecessor successor)
             (add-dependency graph :predecessors predecessor successor))
    graph))

(defun first-before-second-p (first-page second-page sequence)
  (or (not (member first-page sequence))
      (not (member second-page sequence))
      (< (position first-page sequence)
         (position second-page sequence))))

(defun validate-update-sequence (updates graph)
  (let ((predecessors (gethash :predecessors graph))
        (successors (gethash :successors graph)))
    (loop for page in updates
          always (and (loop for pred in (gethash page predecessors)
                            always (first-before-second-p pred page updates))
                      (loop for succ in (gethash page successors)
                            always (first-before-second-p page succ updates))))))

(defun validate-updates (updates graph)
  (loop for update in updates
        if (validate-update-sequence update graph)
          collect (nth (floor (/ (length update) 2)) update) into valid-middles
        else
          collect update into invalid-updates
        finally (return (values valid-middles invalid-updates))))

(defun no-incoming-edge-p (page graph)
  (let ((predecessors (gethash :predecessors graph)))
    (if (null (gethash page predecessors)) t)))

(defun find-nodes-without-incoming-edges (updates graph)
  (loop for page in updates
        when (no-incoming-edge-p page graph)
          collect page into nodes-without-incoming-edges
        finally (return nodes-without-incoming-edges)))

(defun solve (filename)
  (multiple-value-bind (rules updates) (read-rules-and-updates-from filename)
    (let ((graph (create-dependency-graph rules)))
      (multiple-value-bind (valids) (validate-updates updates graph)
        (format t "~a" (reduce #'+ valids))))))


