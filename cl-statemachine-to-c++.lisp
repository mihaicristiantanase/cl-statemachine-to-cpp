;;;; cl-statemachine-to-c++.lisp

;;; TODO: beautify code (ex: join "else", "catch" lines)
;;; TODO: remove trailing white spaces
;;; TODO: cleanup common lisp code
;; TODO(mihai): are types leaking outside StateMachine class?

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
    action-error))

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
  (to-const-sym s "err"))

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

(defmacro define-c++-cont (firstline &rest body)
  `(progn
     (wl ,firstline)
     (let ((*indent* (+ *indent* 2)))
       ,@body
       )))

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
            (cdr c) (cdr c)))))

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
  (wl)
  (define-c++-class "StateMachine"
    (define-c++-enum "TriBool" '((unknown . -1) (false . 0) (true . 1)))
    ;; TODO(mihai): make Err an std::exception
    (define-c++-enum "Err" *errors*)
    (define-c++-doc "The states of the state machine. A state fully defines properties necessary to decide user actions.")
    (define-c++-enum "State" (get-states *machine*))
    (define-c++-doc "The actions of the state machine. An action connects two states.")
    (define-c++-enum "Action" (slot-value *machine* 'actions))
    (wl "typedef int Error;")
    (wl "typedef std::function<void(bool, Error)> Completion;")
    (wl "typedef std::function<void(Completion)> ActionExecutor;")
    (wl "typedef std::tuple<State, Action, State> Transition;")
    (wlb "typedef std::function<TriBool()> Decision;")
    (define-c++-doc "Flag to indicate whether or not this class prints debugging messages.")
    (wlb "bool isLogEnabled = false;")
    (define-c++-doc "Current state.")
    (wlb "State state;")
    (define-c++-doc "Last action.")
    (wlb "Action lastAction;")
    (define-c++-doc "Last action error.")
    (wlb "Err lastActionError;")
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
    (wl "static StateMachine create() {")
    (wl (format nil "  return StateMachine(~a);"
                (state-const-sym (sym->camelcase (get-start *machine*)))))
    (wlb "}")
    ;; (define-c++-doc "Description of the error from last action.")
    ;; (define-c++-fun "errorDescription" "std::string" ""
    ;;     (define-c++-block (format nil "if (lastActionError != ~a)" (err-const-sym "success"))
    ;;         (define-c++-block "if let err = error as? Err"
    ;;             (define-c++-cont "return \"\\(err)\""
    ;;                 (wl ".replacingOccurrences(of: \"(\", with: \":\")")
    ;;               (wl ".replacingOccurrences(of: \")\", with: \"\")")
    ;;               (wl ".replacingOccurrences(of: \"\\\"\", with: \" \")")
    ;;               (wl ".trimmingCharacters(in: .whitespacesAndNewlines)")))
    ;;       (wl "return error.localizedDescription"))
    ;;   (wl "return \"\""))
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
    (wl)
    ;; (define-c++-doc "Start method. Must be called, otherwise, the state machine is not running.")
    ;; (define-c++-fun "start" ""
    ;;   (wl "// check decisions")
    ;;   (loop-decisions (decision)
    ;;                   (define-c++-block (format nil "if ~a == nil" (sym->decision decision))
    ;;                     (wl (format nil "fatalError(\"Machine not started because decision '~a' is missing\")"
    ;;                                 (sym->camelcase decision)))))
    ;;   (wl)
    ;;   (wl "// check actions")
    ;;   (dolist (action (slot-value *machine* 'actions))
    ;;     (define-c++-block (format nil "if action~a == nil" (sym->pascalcase action))
    ;;       (wl (format nil "fatalError(\"Machine not started because action '~a' is missing\")"
    ;;                   (sym->camelcase action)))))
    ;;   (wl)
    ;;   (wl "// start the machine")
    ;;   (define-c++-block "do"
    ;;       (wl "try moveToState(state)"))
    ;;   (define-c++-block "catch"
    ;;       (wl "fatalError(\"\\(error)\")")))
    ;; (wl)
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
      (wl)
      ;; (wl "do {")
      ;; (wl "  let transition = try findTransition(action)")
      ;; (wl "  if actionExec == nil {")
      ;; (wl "    throw Err.transitionNotSet(state, action)")
      ;; (wl "  }")
      ;; (wl "  actionExec {")
      ;; (wl "    [weak self] success, error in")
      ;; (wl "    if error != nil {")
      ;; (wl "      self?.lastActionError = error")
      ;; (wl "      completion(false, error)")
      ;; (wl "      return")
      ;; (wl "    }")
      ;; (wl "    do {")
      ;; (wl "      try self?.moveToState(transition.2)")
      ;; (wl "      self?.lastActionError = error")
      ;; (wl "      completion(success, error)")
      ;; (wl "    }")
      ;; (wl "    catch {")
      ;; (wl "      self?.lastActionError = error")
      ;; (wl "      completion(false, error)")
      ;; (wl "    }")
      ;; (wl "  }")
      ;; (wl "}")
      ;; (wl "catch {")
      ;; (wl "  lastActionError = error")
      ;; (wl "  completion(false, error)")
      ;; (wl "}")
      )
    ;; (wl)
    ;; (wl "private func findTransition(_ action: Action) throws -> Transition {")
    ;; (wl "  for t in transitions {")
    ;; (wl "    if t.0 == state, t.1 == action {")
    ;; (wl "      return t")
    ;; (wl "    }")
    ;; (wl "  }")
    ;; (wl "  throw Err.impossibleAction(state, action)")
    ;; (wlb "}")
    ;; (define-c++-block "private func moveToState(_ state: State) throws"
    ;;   (wl "self.state = state")
    ;;   (wlb "log(\"moveToState \\(state)\")")
    ;;   (define-c++-block "switch state"
    ;;       (dolist (state (get-states *machine*))
    ;;         (wl (format nil "case .~a:" (sym->camelcase state)))
    ;;         (unless (stable-state state *machine*)
    ;;           (loop for decision in (get-unstable-state-decisions state *machine*)
    ;;                 for i from 0 do
    ;;                   (if (listp decision)
    ;;                       (let ((sd (sym->decision (car decision))))
    ;;                         (define-c++-block (format nil "~:[~;else ~]if ~a() ?? false" (> i 0) sd)
    ;;                           (wl (format nil "try moveToState(.~a)" (sym->camelcase (cdr decision))))))
    ;;                       (define-c++-block "else"
    ;;                         (wl (format nil "try moveToState(.~a)" (sym->camelcase decision)))))))
    ;;         (wl "break"))))
    ;; (wl)
    (define-c++-fun "log" "void" "std::string msg"
      (define-c++-block "if (isLogEnabled)"
          (wl "std::cout << \"StateMachine: \" << msg << std::endl;")))
    ))

(defun gen-usage-stream ()
  ;; (define-c++-class "StateMachineTest"
  ;;     (define-c++-fun "test" "void" ""
  ;;       (wl "let sm = StateMachine.create()")
  ;;       (loop-decisions (decision)
  ;;                       (wl (format nil "sm.setDecision~a { [weak self] in /*TODO*/ self?.tautology() }"
  ;;                                   (sym->pascalcase decision))))
  ;;       (dolist (action (slot-value *machine* 'actions))
  ;;         (wl (format nil "sm.setAction~a { [weak self] in self?.~a~a($0) }"
  ;;                     (sym->pascalcase action)
  ;;                     (sym->camelcase action)
  ;;                     (sym->pascalcase (slot-value *machine* 'context)))))
  ;;       (wl "sm.start()"))
  ;;   (dolist (action (slot-value *machine* 'actions))
  ;;     (let ((func-name (format nil "~a~a"
  ;;                               (sym->camelcase action)
  ;;                               (sym->pascalcase (slot-value *machine* 'context)))))
  ;;       (define-c++-fun func-name "_ completion: StateMachine.Completion"
  ;;         (wl (format nil "// TODO: add logic for ~a" func-name)))))
  ;;   (define-c++-block "private func tautology() -> Bool"
  ;;     (wl "return true")))
  (define-c++-fun "main" "int" "int argc, char** argv"
    ;; (wl "StateMachineTest().test()")
    (wl "return 0;")))

(defun gen-code ()
  (with-output-to-string (*stream*)
    (gen-code-stream)))

(defun gen-usage ()
  (with-output-to-string (*stream*)
    (gen-usage-stream)))

(defun save-and-check-c++ (machine path-code path-usage)
  (format t "~&Generating code…~%")
  (let* ((*machine* machine)
         (code (gen-code))
         (code-usage (gen-usage)))
    (with-open-file (f path-code :direction :output :if-exists :supersede)
      (write-string code f))
    (with-open-file (f path-usage :direction :output :if-exists :supersede)
      (write-string code-usage f))
    (format t "~&Verifying output by compiling…~%")
    (format t "~&~a~%" (shell:run t "g++" "-std=c++11" path-code path-usage))
    (format t "~&Done. Please check ~a for the generated C++ file~% ~
                 and ~a for a sample code of how to use."
            path-code path-usage)))
