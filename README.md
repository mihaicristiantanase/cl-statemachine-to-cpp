# cl-statemachine-to-c++

_Mihai Cristian Tănase <mihaicristian.tanase@gmail.com>_

This is a project to generate a StateMachine C++ class from a state machine described in Common Lisp.

## License

[MIT License](LICENSE.md)

## Usage

From the  following BMPN diagram:
![img](demo_ex.png)

*manually* writing the the following code

```lisp
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
             (c-decision
              (flag-c1 . e)
              (flag-c2 . f)
              g)
             d
             e
             f
             g)
   :transitions '((d go-to-g g)
                  (g go-to-a a)
                  (g execute-something nil)
                  (a go-to-b b)
                  (e go-to-f f))))

(cl-statemachine-to-c++:save-and-check-c++ machine
  "/tmp/StateMacpphine.cpp"
  "/tmp/StateMacpphineUsage.cpp")
```

* generates the main state macpphine file at `/tmp/StateMacpphine.cpp`:

```c++
<!-- TODO(mihai): fix this -->
```

* and the usage file at `/tmp/StateMacpphineUsage.cpp`:

```c++
<!-- TODO(mihai): fix this -->
```

## TODO

1. Generate the Machine object automatically by parsing the BMPN diagram;

1. Beautify code (ex: join "else", "catch" lines);

1. Remove trailing white spaces;

1. Cleanup Common Lisp code;
