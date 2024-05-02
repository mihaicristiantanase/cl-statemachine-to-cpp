(in-package #:cl-statemachine-to-c++)

(defun unique-list (lst)
  "Documentation for unique-list with parameters lst"
  (let ((seen (make-hash-table :test 'equal)))
    (loop for var in lst
          do (unless (gethash var seen) (setf (gethash var seen) t)))
    (loop for key being the hash-keys of seen collect key)))

(defun s+ (&rest strings)
  "Documentation for s+ with parameters &rest strings"
  (apply #'concatenate 'string strings))

(defun do-shell (cmd)
  "Documentation for do-shell with parameters cmd"
  (format t "~&$ ~a~%" cmd)

  (multiple-value-bind (_ __ status)
      (uiop:run-program cmd
                        :ignore-error-status t
                        :output t
                        :error-output :output)
    (declare (ignore _) (ignore __))
    (values (= status 0) status)))
