(in-package :pwi/aoc24)

(require :uiop)

(defun try-adding (target numbers acc)
  (cond
    ((null numbers)
     (= target acc))
    ((null (cdr numbers))
     (or (= target (+ acc (car numbers)))
         (try-multiplying target numbers acc)))
    (t
     (let ((new-acc (+ (car numbers) acc)))
       (if (> new-acc target)
           nil
           (or (try-adding target (cdr numbers) new-acc)
               (try-multiplying target (cdr numbers) new-acc)
               (try-concatenating target (cdr numbers) new-acc)))))))

(defun try-multiplying (target numbers acc)
  (cond
    ((null numbers)
     (= target acc))
    ((null (cdr numbers))
     (= target (* acc (car numbers))))
    (t
     (let ((new-acc (* (car numbers) acc)))
       (if (> new-acc target)
           nil
           (or (try-adding target (cdr numbers) new-acc)
               (try-multiplying target (cdr numbers) new-acc)
               (try-concatenating target (cdr numbers) new-acc)))))))

(defun number-of-digits (number)
  (loop for i = number then (floor i 10)
        until (zerop i)
        count 1))

(defun || (number-one number-two)
  (+ (* number-one (expt 10 (number-of-digits number-two))) number-two))

(defun try-concatenating (target numbers acc)
  (cond
    ((null numbers)
     (= target acc))
    ((null (cdr numbers))
     (= target (|| acc (car numbers))))
    (t
     (let ((new-acc (|| acc (car numbers))))
       (if (> new-acc target)
           nil
           (or (try-adding target (cdr numbers) new-acc)
               (try-multiplying target (cdr numbers) new-acc)
               (try-concatenating target (cdr numbers) new-acc)))))))

(defun read-target-and-numbers-from-file (filename)
  "Returns a list of lists. The first entry in each list is the target
   test value; the remainder of the list is that which is needed to compute
   the target value."
  (let ((lines (uiop:with-safe-io-syntax () (uiop:read-file-lines filename))))
    (mapcar #'parse-line lines)))

(defun parse-line (line)
  (destructuring-bind (target-str numbers-str)
      (uiop:split-string line :separator '(#\:))
    (cons
     (parse-integer (string-trim '(#\Space) target-str))
     (mapcar #'parse-integer (uiop:split-string (string-trim '(#\Space) numbers-str))))))

(defun solve (filename)
  (let ((lines (read-target-and-numbers-from-file filename)))
    (loop for (target . numbers) in lines
          when (try-adding target numbers 0)
            collect target into total-calibration-result
          finally (return (reduce #'+ total-calibration-result)))))
