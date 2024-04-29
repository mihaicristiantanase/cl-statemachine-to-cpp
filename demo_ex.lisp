(ql:quickload "cl-statemachine-to-c++")

(defparameter machine
  (make-instance
   'cl-statemachine-to-c++:Machine
   :context 'demo-ex
   :states '((a-decision
              (flag-a . a)
              (flag-b . b)
              (flag-c . c-decision)
              d)
             a
             b
             (c-decision . ((flag-c1 . e) (flag-c2 . f) g))
             d
             e
             f
             g)
   :actions '(go-to-b go-to-a go-to-f go-to-g execute-something)
   :transitions '((d go-to-g g)
                  (g go-to-a a)
                  (g execute-something nil)
                  (a go-to-b b)
                  (e go-to-f f))))

(cl-statemachine-to-c++:save-and-check-c++ machine
  "/tmp/StateMacpphine.cpp"
  "/tmp/StateMacpphineUsage.cpp")