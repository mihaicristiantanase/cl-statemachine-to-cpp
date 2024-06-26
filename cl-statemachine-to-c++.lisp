;;;; cl-statemachine-to-c++.lisp

;;; TODO: beautify code (ex: join "else", "catch" lines)
;;; TODO: cleanup common lisp code
;; TODO(mihai): make decision states consistent by putting the various
;;  branches into transitions
;;  (c-decision . ((flag-c1 . e) (flag-c2 . f) g)) →
;;    in states (c-decision)
;;    in transitions (c-decision flag-c1 e)
;;                   (c-decision flag-c2 f)
;;                   (c-decision t g)
;; TODO(mihai): fix indentation in enum

(in-package #:cl-statemachine-to-c++)

(defclass Machine ()
  ((context
    :reader context
    :initarg :context)
   (states
    :initarg :states
    :reader states
    :initform nil)
   (transitive-states
    :initarg :transitive-states
    :reader transitive-states
    :initform nil)
   (transitions
    :initarg :transitions
    :reader transitions
    :initform nil)
   (actions
    :accessor actions
    :initform nil)))

(defparameter *machine* nil)

(defparameter *errors*
  '(success
    impossible-action
    transition-not-set
    invalid-transition
    invalid-decision
    no-actions-for-state
    general-error))

(defparameter *indent* 0)
(defparameter *stream* nil)

(defun decision-state-p (state machine)
  (null (find state (states machine))))

(defun transitive-state-p (state machine)
  (find state (transitive-states machine)))

(defgeneric get-start (machine))
(defgeneric get-states (machine))
(defgeneric get-unstable-state-decisions (state machine))

(defmethod initialize-instance :after ((machine Machine) &key)
  (let ((states (mapcar #'(lambda (s) (if (consp s) (car s) s)) (states machine))))
    ;; validate transitive states
    (let ((bad-states (set-difference (transitive-states machine) states)))
      (when bad-states
        (error "Found transitive states ~a that are not defined as states" bad-states)))
    (let ((bad-states
            (remove-if #'(lambda (item) (= (length (cdr item)) 1))
                       (mapcar #'(lambda (s)
                                   (cons s (remove-if-not #'(lambda (item) (eq s item))
                                                          (transitions machine)
                                                          :key #'car)))
                               (transitive-states machine)))))
      (when bad-states
        (error "Found transitive states ~a with more than one transition" bad-states)))
    ;; validate states in transitions
    (let ((bad-states (set-difference
                       (apply #'append (mapcar
                                        #'(lambda (x) (append (list (car x))
                                                              (when (caddr x) (list (caddr x)))))
                                        (transitions machine)))
                       states)))
      (when bad-states
        (error "Found states in transtions ~a that are not defined as states" bad-states))))
  ;; fill transition to same state
  (dolist (transition (transitions machine))
    (unless (caddr transition)
      (setf (caddr transition) (car transition))))
  ;; auto-fill actions
  (setf (actions machine)
        (sort (unique-list
               (mapcar #'second (transitions machine)))
              #'string<
              :key #'symbol-name)))

(defmethod get-start ((machine Machine))
  (car (get-states machine)))

(defmethod get-states ((machine Machine))
  (mapcar #'(lambda (s) (if (symbolp s) s (car s)))
          (states machine)))

(defmethod get-unstable-state-decisions (state (machine Machine))
  (dolist (s (states machine))
    (when (and (listp s) (eq (car s) state))
      (return-from get-unstable-state-decisions (cdr s)))))

(defun wl (&optional (fmt "") &rest body)
  "Write line"
  (when (> (length fmt) 0)
      (format *stream* (make-string *indent* :initial-element #\Space)))
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
         (when (decision-state-p state *machine*)
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
  (let ((cls-name (s+ (sym->pascalcase (context *machine*)) "StateMachine")))
    (define-c++-class cls-name
        (define-c++-class-section "public")
      (define-c++-doc "The states of the state machine. A state fully defines properties necessary to decide user actions.")
      (define-c++-enum "State" (get-states *machine*))
      (define-c++-doc "The actions of the state machine. An action connects two states.")
      (define-c++-enum "Action" (actions *machine*))
      (define-c++-enum "ErrId" *errors*)
      (wl "typedef std::function<void(bool)> Completion;")
      (wl "typedef std::function<void(Completion)> ActionExecutor;")
      (wl "typedef std::tuple<State, Action, State> Transition;")
      (wlb "typedef std::function<bool()> Decision;")
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
            (define-c++-block "Err(ErrId err, State state) : err(err)"
                (wl "this->message = enumNameForErrId(err) +")
              (wl "\" state:\" + enumNameForState(state);"))
            (define-c++-block "Err(std::string message) : err(kErrIdGeneralError)"
                (wl "this->message = enumNameForErrId(err) + \" \" + message;"))
            (define-c++-block "Err(const char* message) : err(kErrIdGeneralError)"
                (wl "this->message = enumNameForErrId(err) + \" \" +std::string(message);"))
            (define-c++-block "virtual const char* what() const noexcept"
                (wl "return message.c_str();"))))
      (wlb "bool isLogEnabled = false;")
      (wl "static ~a create() {" cls-name)
      (wl "  return ~a(~a);"
          cls-name
          (state-const-sym (sym->camelcase (get-start *machine*))))
      (wlb "}")
      (define-c++-doc "Description of the error from last action.")
      (define-c++-fun "errorDescription" "std::string" ""
        (define-c++-block "if (lastActionError)"
            (wl "const char* rv = lastActionError->what();")
          (wl "return std::string(rv);"))
        (wl "return \"\";"))
      (loop-decisions (decision)
                      (define-c++-doc (format nil "Set decision for ~a" (sym->decision decision)))
                      (define-c++-fun (format nil "setDecision~a" (sym->pascalcase decision)) "void"
                        "Decision decision"
                        (wl (format nil "~a = decision;" (sym->decision decision)))))
      (dolist (action (actions *machine*))
        (let ((ap (sym->pascalcase action))
              (ac (sym->camelcase action)))
          (define-c++-doc (format nil "Set action ~a" (action-const-sym ac)))
          (define-c++-fun (format nil "setAction~a" ap) "void" "ActionExecutor action"
            (wl (format nil "action~a = action;" ap)))
          (define-c++-doc (format nil "Execute action ~a from current state"
                                  (action-const-sym ac)))
          (define-c++-fun (format nil "doAction~a" ap) "void" "Completion completion"
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
        (dolist (action (actions *machine*))
          (define-c++-block (format nil "if (!action~a)" (sym->pascalcase action))
              (wl (format nil "throw Err(\"Machine not started because action '~a' is missing\");"
                          (sym->camelcase action)))))
        (wl)
        (wl "// start the machine")
        (define-c++-block "try"
            (wl "moveToState(state, [](bool){});"))
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
      (dolist (action (actions *machine*))
        (wl "ActionExecutor action~a;" (sym->pascalcase action)))
      (wl)
      (define-c++-doc "Decisions")
      (loop-decisions (decision)
                      (wl "Decision ~a;" (sym->decision decision)))
      (wl)
      (define-c++-doc "Transitions")
      (let ((transitions (transitions *machine*)))
        (wl "Transition transitions[~a] = {" (length transitions))
        (dolist (trans transitions)
          (wl "  {~{~a~^, ~}}," (list (state-const-sym (nth 0 trans))
                                      (action-const-sym (nth 1 trans))
                                      (state-const-sym (nth 2 trans)))))
        (wlb "};"))
      (wl "~a(State state) {" cls-name)
      (wl "  this->state = state;")
      (wlb "}")
      (define-c++-fun "doAction" "void " "Action action, Completion completion"
        (wl "log(\"doAction \" + enumNameForAction(action));")
        (wl "lastAction = action;")
        (wl)
        (wl "ActionExecutor actionExec;")
        (define-c++-block "switch (action)"
            (dolist (action (actions *machine*))
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
             (define-c++-block "actionExec([&](bool success)"
                 (define-c++-block "if (!success)"
                     (wl "completion(false);")
                   (wl "return;"))
               (define-c++-try
                   ((wl "moveToState(std::get<2>(transition), completion);"))
                   ((wl "lastActionError = &e;")
                    (wl "completion(false);"))))
             (wl ");"))
            ((wl "lastActionError = &e;")
             (wl "completion(false);"))))
      (define-c++-fun "findTransition" "Transition" "Action action"
        (define-c++-block "for (auto cand : transitions)"
            (wl "if (std::get<0>(cand) == state && std::get<1>(cand) == action) {")
          (wl "  return cand;")
          (wl "}"))
        (wl "throw Err(kErrIdImpossibleAction, state, action);"))
      (define-c++-fun "getOnePossibleAction" "Action" "State state"
        (define-c++-block "for (auto cand : transitions)"
            (wl "if (std::get<0>(cand) == state) {")
          (wl "  return std::get<1>(cand);")
          (wl "}"))
        (wl "throw Err(kErrIdNoActionsForState, state);"))
      (define-c++-fun "moveToState" "void" "State state, Completion completion"
        (wl "this->state = state;")
        (wlb "log(\"moveToState \" + enumNameForState(state));")
        (define-c++-block "switch (state)"
            (dolist (state (get-states *machine*))
              (wl (format nil "case ~a:" (state-const-sym (sym->camelcase state))))
              (cond ((decision-state-p state *machine*)
                     (loop for decision in (get-unstable-state-decisions state *machine*)
                           for i from 0 do
                             (if (listp decision)
                                 (let ((sd (sym->decision (car decision))))
                                   (define-c++-block (format nil "~:[~;else ~]if (~a())" (> i 0) sd)
                                       (wl (format nil "moveToState(~a, completion);"
                                                   (state-const-sym (sym->camelcase (cdr decision)))))))
                                 (define-c++-block "else"
                                     (wl (format nil "moveToState(~a, completion);"
                                                 (state-const-sym (sym->camelcase decision))))))))
                    ((transitive-state-p state *machine*)
                     (wl "doAction(getOnePossibleAction(~a), completion);"
                         (state-const-sym (sym->camelcase state))))
                    (t (wl "completion(true);")))
              (wl "break;"))))
      (define-c++-fun "log" "void" "std::string msg"
        (define-c++-block "if (isLogEnabled)"
            (wl "std::cout << \"~a: \" << msg << std::endl;" cls-name))))))

(defun gen-usage-stream (path-code)
  (wl (format nil "#include \"~a\"" (file-name path-code)))
  (wl)
  (let* ((sm-name (s+ (sym->pascalcase (context *machine*)) "StateMachine"))
         (cls-name (s+ sm-name "Test")))
    (define-c++-class cls-name
        (define-c++-class-section "public"
            (define-c++-fun "test" "void" ""
              (wl "~a sm = ~a::create();" sm-name sm-name)
              (wl "sm.isLogEnabled = true;")
              (loop-decisions (decision)
                              (wl (format nil "sm.setDecision~a([&]() { /*TODO*/ return falsity(); });"
                                          (sym->pascalcase decision))))
              (dolist (action (actions *machine*))
                (wl (format nil "sm.setAction~a([&](~a::Completion completion) { ~a~a(completion); });"
                            (sym->pascalcase action)
                            sm-name
                            (sym->camelcase action)
                            (sym->pascalcase (context *machine*)))))
              (wl "sm.start();")
              (wl)
              (wl "std::cout << \"-- Testing out a few actions:\" << std::endl;")
              (dolist (action (actions *machine*))
                (define-c++-block (format nil "sm.doAction~a([&](bool success)"
                                          (sym->pascalcase action))
                    (wl "std::cout << \"-- success:\" << success << std::endl;")
                  (wl "std::cout << \"-- error:\" << sm.errorDescription() << std::endl;"))
                (wl ");"))))
      (define-c++-class-section "private"
          (dolist (action (actions *machine*))
            (let ((func-name (format nil "~a~a"
                                     (sym->camelcase action)
                                     (sym->pascalcase (context *machine*)))))
              (define-c++-fun func-name "void" (s+ sm-name "::Completion completion")
                (wl (format nil "// TODO: add logic for ~a" func-name))
                (wl "std::cout << \"-- ~a: ~a\" << std::endl;" cls-name func-name)
                (wl "completion(true);"))))
        (define-c++-fun "falsity" "bool" ""
          (wl "return false;"))))
    (define-c++-fun "main" "int" "int argc, char** argv"
      (wl "~a().test();" cls-name)
      (wl "return 0;"))))

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
