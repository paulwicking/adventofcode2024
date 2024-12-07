(in-package :pwi/aoc24)

(defun valid-increase (a b)
  (and (< a b)
       (<= (- b a) 3)))

(defun valid-decrease (a b)
  (and (> a b)
       (<= (- a b) 3)))

(defun check-secureness (lst)
  (or (all-increasing lst) (all-decreasing lst)))

(defun check-order (lst predicate)
  (loop for (a b) on lst
        while b
        always (funcall predicate a b)))

(defun all-increasing (lst)
  (check-order lst #'valid-increase))

(defun all-decreasing (lst)
  (check-order lst #'valid-decrease))

(defun all-increasing-dampened (lst)
  (check-dampened lst #'valid-increase))

(defun all-decreasing-dampened (lst)
  (check-dampened lst #'valid-decrease))


(defun string-to-list (str)
  (with-input-from-string (s str)
    (loop for num = (read s nil nil)
          while num
          collect num)))

(defun read-input (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
          while line
          count (secure-when-dampened (string-to-list line)))))

(defun secure-when-dampened (lst)
  (or (or (all-increasing lst)
          (all-increasing-dampened lst))
      (or (all-decreasing lst)
          (all-decreasing-dampened lst))))

(defun check-dampened (lst predicate)
  (loop for (a b) on lst
        for index from 0
        while b
        unless (funcall predicate a b)
          do (let ((without-a (append (subseq lst 0 index)
                                      (subseq lst (1+ index))))
                   (without-b (append (subseq lst 0 (1+ index))
                                      (subseq lst (+ index 2)))))
               (unless (or (check-secureness without-a)
                           (check-secureness without-b))
                 (return nil)))
        finally (return t)))

