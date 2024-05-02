;;;; cl-statemachine-to-c++.lisp

;;; TODO: beautify code (ex: join "else", "catch" lines)
;;; TODO: remove trailing white spaces
;;; TODO: cleanup common lisp code

(in-package #:cl-statemachine-to-c++)

(defclass Machine ()
  ((context
    :initarg :context)
   (states
    :initarg :states
    :initform nil)
   (transitions
    :initarg :transitions
    :initform nil)
   (actions
    :initform nil)))

(defparameter *machine* nil)

(defparameter *errors*
  '(success
    impossible-action
    transition-not-set
    invalid-transition
    invalid-decision
    general-error))

(defparameter *indent* 0)
(defparameter *stream* nil)

(defun stable-state (state machine)
  (not (null (find state (slot-value machine 'states)))))

(defgeneric get-start (machine))
(defgeneric get-states (machine))
(defgeneric get-unstable-state-decisions (state machine))

(defmethod initialize-instance :after ((machine Machine) &key)
  (dolist (transition (slot-value machine 'transitions))
    (unless (caddr transition)
      (setf (caddr transition) (car transition))))
  ;; auto-fill actions
  (setf (slot-value machine 'actions)
        (sort (unique-list
               (mapcar #'second (slot-value machine 'transitions)))
              #'string<
              :key #'symbol-name)))

(defmethod get-start ((machine Machine))
  (car (get-states machine)))

(defmethod get-states ((machine Machine))
  (mapcar #'(lambda (s) (if (symbolp s) s (car s)))
          (slot-value machine 'states)))

(defmethod get-unstable-state-decisions (state (machine Machine))
  (dolist (s (slot-value machine 'states))
    (when (and (listp s) (eq (car s) state))
      (return-from get-unstable-state-decisions (cdr s)))))

(defun wl (&optional (fmt "") &rest body)
  "Write line"
  (format *stream* (make-string *indent* :initial-element #\Space))
  (apply #'format *stream* fmt body)
  (format *stream* "~%"))

(defun wlb (&optional (fmt "") &rest body)
  "Write Line Block"
  (wl fmt body)
  (wl))

(defun sym->camelcase (sym )
  (cl-change-case:camel-case (symbol-name sym)))

(defun sym->pascalcase (sym )
  (cl-change-case:pascal-case (symbol-name sym)))

(defun sym->decision (sym)
  (format nil "is~a" (sym->pascalcase sym)))

(defun to-const-sym (s &optional prefix)
  "Documentation for to-const-sym with parameters s"
  (cl-change-case:camel-case
   (format nil "k ~@[~a~] ~a" prefix s)))

(defun state-const-sym (s)
  "Documentation for state-const-sym with parameters s"
  (to-const-sym s "State"))

(defun action-const-sym (s)
  "Documentation for action-const-sym with parameters s"
  (to-const-sym s "Action"))

(defun err-const-sym (s)
  "Documentation for err-const-sym with parameters s"
  (to-const-sym s "ErrId"))

(defun define-c++-doc (doc)
  (wl "/**")
  (wl " * ~a" doc)
  (wl " */"))

(defun semicolon-block-p (block-first-line)
  "Documentation for semicolon-block-p with parameters block-first-line"
  (or (search "class" block-first-line)
      (search "enum" block-first-line)))

(defmacro define-c++-block (firstline &rest body)
  `(progn
     (wl (concatenate 'string ,firstline " {"))
     (let ((*indent* (+ *indent* 2)))
       ,@body)
     (wl "}~:[~;;~]" (semicolon-block-p ,firstline))
     (wl)))

(defmacro define-c++-class-section (section-name &rest body)
  `(progn
     (let ((*indent* (- *indent* 2)))
       (wl (s+ ,section-name ":")))
     ,@body))

(defmacro define-c++-try (try-block catch-block)
  `(progn
     (wl (s+ "try {"))
     (let ((*indent* (+ *indent* 2)))
       ,@try-block)
     (wl (s+ "} catch (std::exception& e) {"))
     (let ((*indent* (+ *indent* 2)))
       ,@catch-block)
     (wl "}")
     (wl)))

(defmacro define-c++-cont (firstline &rest body)
  `(progn
     (wl ,firstline)
     (let ((*indent* (+ *indent* 2)))
       ,@body)))

(defmacro define-c++-sfun (name params &rest body)
  `(define-c++-block (format nil "static func ~a(~a)" ,name ,params) ,@body))

(defmacro define-c++-fun (name ret params &rest body)
  `(define-c++-block (format nil "~a ~a(~a)" ,ret ,name ,params) ,@body))

(defun define-c++-enum (name cases)
  (define-c++-block (s+ "enum " name)
      (dolist (c cases)
        (unless (consp c) (setf c (cons c nil)))
        (wl "  ~a~:[~; = ~a~],"
            (to-const-sym (sym->camelcase (car c)) name)
            (cdr c) (cdr c))))
  (define-c++-fun (s+ "enumNameFor" name)
    "static std::string" (s+ name " item")
    (define-c++-block "switch (item)"
      (dolist (c cases)
        (let ((casename (to-const-sym (sym->camelcase (if (consp c) (car c) c))
                                      name)))
          (wl "case ~a:" casename)
          (wl "return \"~a.~a\";" name casename))))))

(defmacro define-c++-class (name &rest body)
  `(define-c++-block (format nil "class ~a" ,name) ,@body))

(defmacro loop-decisions ((varname) &rest body)
  (let ((v (gensym)))
    `(let (,v)
       (dolist (state (get-states *machine*))
         (unless (stable-state state *machine*)
           (loop for ,varname in (get-unstable-state-decisions state *machine*)
                 when (listp ,varname) do
                   (let ((,varname (car ,varname)))
                     (unless (find ,varname ,v)
                       (push ,varname ,v)
                       ,@body))))))))

(defun gen-code-stream ()
  (wl "//")
  (wl "// This file is generated with cl-statemachine-to-c++")
  (wl "// Changes are not recommended.")
  (wl "//")
  (wl)
  (wl "#include <iostream>")
  (wl "#include <functional>")
  (wl "#include <exception>")
  (wl)
  (define-c++-class "StateMachine"
    (define-c++-class-section "public")
    (wl "typedef std::function<void(bool, std::exception*)> Completion;")
    (wl)
    (wlb "bool isLogEnabled = false;")

    (define-c++-doc "The states of the state machine. A state fully defines properties necessary to decide user actions.")
    (define-c++-enum "State" (get-states *machine*))
    (define-c++-doc "The actions of the state machine. An action connects two states.")
    (define-c++-enum "Action" (slot-value *machine* 'actions))
    (define-c++-enum "ErrId" *errors*)
    (define-c++-class "Err : public std::exception"
        (define-c++-class-section "public"
          (wl "ErrId err;")
          (wl "std::string message;")
          (wl)
          (define-c++-block "Err() : err(kErrIdSuccess)")
          (define-c++-block "Err(ErrId err, State state, Action action) : err(err)"
            (wl "this->message = enumNameForErrId(err) +")
            (wl "\" state:\" + enumNameForState(state) +")
            (wl "\" action:\" + enumNameForAction(action);"))
          (define-c++-block "Err(std::string message) : err(kErrIdGeneralError)"
            (wl "this->message = enumNameForErrId(err) + \" \" + message;"))
          (define-c++-block "Err(const char* message) : err(kErrIdGeneralError)"
            (wl "this->message = enumNameForErrId(err) + \" \" +std::string(message);"))
          (define-c++-block "virtual const char* what() const noexcept"
            (wl "return message.c_str();"))))
    (wl "typedef std::function<void(Completion)> ActionExecutor;")
    (wl "typedef std::tuple<State, Action, State> Transition;")
    (wlb "typedef std::function<bool()> Decision;")
    (wl "static StateMachine create() {")
    (wl (format nil "  return StateMachine(~a);"
                (state-const-sym (sym->camelcase (get-start *machine*)))))
    (wlb "}")
    (define-c++-doc "Description of the error from last action.")
    (define-c++-fun "errorDescription" "std::string" ""
      (define-c++-block "if (lastActionError)"
        (wl "const char* rv = lastActionError->what();")
        (wl "return std::string(rv);"))
      (wl "return \"\";"))
    (loop-decisions (decision)
                    (wl)
                    (define-c++-doc (format nil "Set decision for ~a" (sym->decision decision)))
                    (define-c++-fun (format nil "setDecision~a" (sym->pascalcase decision)) "void"
                      "Decision decision"
                      (wl (format nil "~a = decision;" (sym->decision decision)))))
    (dolist (action (slot-value *machine* 'actions))
      (let ((ap (sym->pascalcase action))
            (ac (sym->camelcase action)))
        (wl)
        (define-c++-doc (format nil "Set action ~a" (action-const-sym ac)))
        (define-c++-fun (format nil "setAction~a" ap) "void" "ActionExecutor action"
          (wl (format nil "action~a = action;" ap)))
        (define-c++-doc (format nil "Execute action ~a from current state"
                                (action-const-sym ac)))
        (define-c++-fun (format nil "doAction~a" ap) "void" "Completion completion"
          (wl (format nil "log(\"doAction~a\");" ap))
          (wl (format nil "doAction(~a, completion);" (action-const-sym ac))))))
    (define-c++-doc "Inspect the current state.")
    (define-c++-fun "getState" "State" ""
      (wl "return state;"))
    (define-c++-doc "Inspect the last action executed.")
    (define-c++-fun "getLastAction" "Action" ""
      (wl "return lastAction;"))
    (define-c++-doc "Inspect the last action error.")
    (define-c++-fun "getLastActionError" "std::exception*" ""
      (wl "return lastActionError;"))
    (define-c++-doc "Start method. Must be called, otherwise, the state machine is not running.")
    (define-c++-fun "start" "void" ""
      (wl "// check decisions")
      (loop-decisions (decision)
                      (define-c++-block (format nil "if (!~a)" (sym->decision decision))
                        (wl (format nil "throw Err(\"Machine not started because decision '~a' is missing\");"
                                    (sym->camelcase decision)))))
      (wl)
      (wl "// check actions")
      (dolist (action (slot-value *machine* 'actions))
        (define-c++-block (format nil "if (!action~a)" (sym->pascalcase action))
          (wl (format nil "throw Err(\"Machine not started because action '~a' is missing\");"
                      (sym->camelcase action)))))
      (wl)
      (wl "// start the machine")
      (define-c++-block "try"
          (wl "moveToState(state);"))
      (define-c++-block "catch (std::exception& e)"
          (wl "throw std::runtime_error(e.what());")))

    (define-c++-class-section "private")
    (define-c++-doc "Flag to indicate whether or not this class prints debugging messages.")
    (define-c++-doc "Current state.")
    (wlb "State state;")
    (define-c++-doc "Last action.")
    (wlb "Action lastAction;")
    (define-c++-doc "Last action error.")
    (wlb "std::exception* lastActionError = NULL;")
    (define-c++-doc "Actions")
    (dolist (action (slot-value *machine* 'actions))
      (wl "ActionExecutor action~a;" (sym->pascalcase action)))
    (wl)
    (define-c++-doc "Decisions")
    (loop-decisions (decision)
                    (wl "Decision ~a;" (sym->decision decision)))
    (wl)
    (define-c++-doc "Transitions")
    (let ((transitions (slot-value *machine* 'transitions)))
      (wl "Transition transitions[~a] = {" (length transitions))
      (dolist (trans transitions)
        (wl "  {~{~a~^, ~}}," (list (state-const-sym (nth 0 trans))
                               (action-const-sym (nth 1 trans))
                               (state-const-sym (nth 2 trans)))))
      (wlb "};"))
    (wl "StateMachine(State state) {")
    (wl "  this->state = state;")
    (wlb "}")
    (define-c++-fun "doAction" "void " "Action action, Completion completion"
      (wl "lastAction = action;")
      (wl)
      (wl "ActionExecutor actionExec;")
      (define-c++-block "switch (action)"
          (dolist (action (slot-value *machine* 'actions))
            (let ((ac (sym->camelcase action))
                  (ap (sym->pascalcase action)))
              (wl (format nil "case ~a:" (action-const-sym ac)))
              (wl (format nil "actionExec = action~a;" ap))
              (wl "break;"))))
      (define-c++-try
          ((wl "Transition transition = findTransition(action);")
           (wl "if (!actionExec) {")
           (wl "  throw Err(kErrIdTransitionNotSet, state, action);")
           (wl "}")
           (define-c++-block "actionExec([&](bool success, std::exception* actionException)"
               (define-c++-block "if (!success)"
                 (wl "lastActionError = actionException;")
                 (wl "completion(false, actionException);")
                 (wl "return;"))
             (define-c++-try
                 ((wl "moveToState(std::get<2>(transition));")
                  (wl "lastActionError = actionException;")
                  (wl "completion(success, actionException);"))
                 ((wl "lastActionError = &e;")
                  (wl "completion(false, &e);"))))
           (wl ");"))
          ((wl "lastActionError = &e;")
           (wl "completion(false, &e);"))))
    (define-c++-fun "findTransition" "Transition" "Action action"
      (define-c++-block "for (auto cand : transitions)"
          (wl "if (std::get<0>(cand) == state && std::get<1>(cand) == action) {")
        (wl "  return cand;")
        (wl "}"))
        (wl "throw Err(kErrIdImpossibleAction, state, action);"))
    (define-c++-fun "moveToState" "void" "State state"
      (wl "this->state = state;")
      (wlb "log(\"moveToState \" + enumNameForState(state));")
      (define-c++-block "switch (state)"
          (dolist (state (get-states *machine*))
            (wl (format nil "case ~a:" (state-const-sym (sym->camelcase state))))
            (unless (stable-state state *machine*)
              (loop for decision in (get-unstable-state-decisions state *machine*)
                    for i from 0 do
                      (if (listp decision)
                          (let ((sd (sym->decision (car decision))))
                            (define-c++-block (format nil "~:[~;else ~]if (~a())" (> i 0) sd)
                              (wl (format nil "moveToState(~a);" (state-const-sym (sym->camelcase (cdr decision)))))))
                          (define-c++-block "else"
                            (wl (format nil "moveToState(~a);" (state-const-sym (sym->camelcase decision))))))))
            (wl "break;"))))
    (define-c++-fun "log" "void" "std::string msg"
      (define-c++-block "if (isLogEnabled)"
          (wl "std::cout << \"StateMachine: \" << msg << std::endl;")))))

(defun gen-usage-stream (path-code)
  (wl (format nil "#include \"~a\"" (file-name path-code)))
  (wl)
  (define-c++-class "StateMachineTest"
      (define-c++-class-section "public"
          (define-c++-fun "test" "void" ""
            (wl "StateMachine sm = StateMachine::create();")
            (wl "sm.isLogEnabled = true;")
            (loop-decisions (decision)
                            (wl (format nil "sm.setDecision~a([&]() { /*TODO*/ return tautology(); });"
                                        (sym->pascalcase decision))))
            (dolist (action (slot-value *machine* 'actions))
              (wl (format nil "sm.setAction~a([&](StateMachine::Completion completion) { ~a~a(completion); });"
                          (sym->pascalcase action)
                          (sym->camelcase action)
                          (sym->pascalcase (slot-value *machine* 'context)))))
            (wl "sm.start();")
            (wl)
            (wl "std::cout << \"-- This returns a specific exception:\" << std::endl;")
            (define-c++-block "sm.doActionExecuteSomething([&](bool success, std::exception* e)"
              (wl "std::cout << \"-- success:\" << success << std::endl;")
              (wl "std::cout << \"-- error:\" << sm.errorDescription() << std::endl;"))
            (wl ");")
            (wl "std::cout << \"-- This moves through various states:\" << std::endl;")
            (define-c++-block "sm.doActionGoToB([&](bool success, std::exception* e)"
              (wl "std::cout << \"-- success:\" << success << std::endl;"))
            (wl ");")))
    (define-c++-class-section "private"
        (dolist (action (slot-value *machine* 'actions))
          (let ((func-name (format nil "~a~a"
                                   (sym->camelcase action)
                                   (sym->pascalcase (slot-value *machine* 'context)))))
            (define-c++-fun func-name "void" "StateMachine::Completion completion"
              (wl (format nil "// TODO: add logic for ~a" func-name))
              (wl "completion(true, NULL);"))))
      (define-c++-fun "tautology" "bool" ""
        (wl "return true;"))))
  (define-c++-fun "main" "int" "int argc, char** argv"
    (wl "StateMachineTest().test();")
    (wl "return 0;")))

(defun save-and-check-c++ (machine path-code path-usage)
  (format t "~&Generating code…~%")
  (let* ((*machine* machine)
         (code (with-output-to-string (*stream*)
                 (gen-code-stream)))
         (code-usage (with-output-to-string (*stream*)
                       (gen-usage-stream path-code))))
    (with-open-file (f path-code :direction :output :if-exists :supersede)
      (write-string code f))
    (with-open-file (f path-usage :direction :output :if-exists :supersede)
      (write-string code-usage f))
    (format t "~&Verifying output by compiling…~%")
    (unless (do-shell (s+ "g++ -std=c++11 -g -o /tmp/sm " path-usage))
      (format t "~&There was an error.~%")
      (return-from save-and-check-c++))
    (unless (do-shell "/tmp/sm")
      (format t "~&There was an error.~%")
      (return-from save-and-check-c++))
    (format t "~&Done. Please check ~a for the generated C++ file~% ~
                 and ~a for a sample code of how to use."
            path-code path-usage)))
