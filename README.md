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
             (c-decision . ((flag-c1 . e) (flag-c2 . f) g))
             d
             e
             f
             g)
   :transitive-states '(a g)
   :transitions '((d go-to-g g)
                  (g go-to-a a)
                  (a go-to-b b)
                  (e go-to-f f))))

(cl-statemachine-to-c++:save-and-check-c++ machine
  "/tmp/Machine.hpp"
  "/tmp/MachineTest.cpp")
```

* generates the main state machine file at `/tmp/Machine.hpp`:

```c++
//
// This file is generated with cl-statemachine-to-c++
// Changes are not recommended.
//

#include <iostream>
#include <functional>
#include <exception>

class StateMachine {
public:
  /**
   * The states of the state machine. A state fully defines properties necessary to decide user actions.
   */
  enum State {
      kStateADecision,
      kStateA,
      kStateB,
      kStateCDecision,
      kStateD,
      kStateE,
      kStateF,
      kStateG,
  };

  static std::string enumNameForState(State item) {
    switch (item) {
      case kStateADecision:
      return "State.kStateADecision";
      case kStateA:
      return "State.kStateA";
      case kStateB:
      return "State.kStateB";
      case kStateCDecision:
      return "State.kStateCDecision";
      case kStateD:
      return "State.kStateD";
      case kStateE:
      return "State.kStateE";
      case kStateF:
      return "State.kStateF";
      case kStateG:
      return "State.kStateG";
    }

  };

  /**
   * The actions of the state machine. An action connects two states.
   */
  enum Action {
      kActionGoToA,
      kActionGoToB,
      kActionGoToF,
      kActionGoToG,
  };

  static std::string enumNameForAction(Action item) {
    switch (item) {
      case kActionGoToA:
      return "Action.kActionGoToA";
      case kActionGoToB:
      return "Action.kActionGoToB";
      case kActionGoToF:
      return "Action.kActionGoToF";
      case kActionGoToG:
      return "Action.kActionGoToG";
    }

  };

  enum ErrId {
      kErrIdSuccess,
      kErrIdImpossibleAction,
      kErrIdTransitionNotSet,
      kErrIdInvalidTransition,
      kErrIdInvalidDecision,
      kErrIdNoActionsForState,
      kErrIdGeneralError,
  };

  static std::string enumNameForErrId(ErrId item) {
    switch (item) {
      case kErrIdSuccess:
      return "ErrId.kErrIdSuccess";
      case kErrIdImpossibleAction:
      return "ErrId.kErrIdImpossibleAction";
      case kErrIdTransitionNotSet:
      return "ErrId.kErrIdTransitionNotSet";
      case kErrIdInvalidTransition:
      return "ErrId.kErrIdInvalidTransition";
      case kErrIdInvalidDecision:
      return "ErrId.kErrIdInvalidDecision";
      case kErrIdNoActionsForState:
      return "ErrId.kErrIdNoActionsForState";
      case kErrIdGeneralError:
      return "ErrId.kErrIdGeneralError";
    }

  };

  typedef std::function<void(bool)> Completion;
  typedef std::function<void(Completion)> ActionExecutor;
  typedef std::tuple<State, Action, State> Transition;
  typedef std::function<bool()> Decision;

  class Err : public std::exception {
  public:
    ErrId err;
    std::string message;

    Err() : err(kErrIdSuccess) {
    }

    Err(ErrId err, State state, Action action) : err(err) {
      this->message = enumNameForErrId(err) +
      " state:" + enumNameForState(state) +
      " action:" + enumNameForAction(action);
    }

    Err(ErrId err, State state) : err(err) {
      this->message = enumNameForErrId(err) +
      " state:" + enumNameForState(state);
    }

    Err(std::string message) : err(kErrIdGeneralError) {
      this->message = enumNameForErrId(err) + " " + message;
    }

    Err(const char* message) : err(kErrIdGeneralError) {
      this->message = enumNameForErrId(err) + " " +std::string(message);
    }

    virtual const char* what() const noexcept {
      return message.c_str();
    }

  };

  bool isLogEnabled = false;

  static StateMachine create() {
    return StateMachine(kStateADecision);
  }

  /**
   * Description of the error from last action.
   */
  std::string errorDescription() {
    if (lastActionError) {
      const char* rv = lastActionError->what();
      return std::string(rv);
    }

    return "";
  }

  /**
   * Set decision for isFlagA
   */
  void setDecisionFlagA(Decision decision) {
    isFlagA = decision;
  }

  /**
   * Set decision for isFlagB
   */
  void setDecisionFlagB(Decision decision) {
    isFlagB = decision;
  }

  /**
   * Set decision for isFlagC
   */
  void setDecisionFlagC(Decision decision) {
    isFlagC = decision;
  }

  /**
   * Set decision for isFlagC1
   */
  void setDecisionFlagC1(Decision decision) {
    isFlagC1 = decision;
  }

  /**
   * Set decision for isFlagC2
   */
  void setDecisionFlagC2(Decision decision) {
    isFlagC2 = decision;
  }

  /**
   * Set action kActionGoToA
   */
  void setActionGoToA(ActionExecutor action) {
    actionGoToA = action;
  }

  /**
   * Execute action kActionGoToA from current state
   */
  void doActionGoToA(Completion completion) {
    doAction(kActionGoToA, completion);
  }

  /**
   * Set action kActionGoToB
   */
  void setActionGoToB(ActionExecutor action) {
    actionGoToB = action;
  }

  /**
   * Execute action kActionGoToB from current state
   */
  void doActionGoToB(Completion completion) {
    doAction(kActionGoToB, completion);
  }

  /**
   * Set action kActionGoToF
   */
  void setActionGoToF(ActionExecutor action) {
    actionGoToF = action;
  }

  /**
   * Execute action kActionGoToF from current state
   */
  void doActionGoToF(Completion completion) {
    doAction(kActionGoToF, completion);
  }

  /**
   * Set action kActionGoToG
   */
  void setActionGoToG(ActionExecutor action) {
    actionGoToG = action;
  }

  /**
   * Execute action kActionGoToG from current state
   */
  void doActionGoToG(Completion completion) {
    doAction(kActionGoToG, completion);
  }

  /**
   * Inspect the current state.
   */
  State getState() {
    return state;
  }

  /**
   * Inspect the last action executed.
   */
  Action getLastAction() {
    return lastAction;
  }

  /**
   * Inspect the last action error.
   */
  std::exception* getLastActionError() {
    return lastActionError;
  }

  /**
   * Start method. Must be called, otherwise, the state machine is not running.
   */
  void start() {
    // check decisions
    if (!isFlagA) {
      throw Err("Machine not started because decision 'flagA' is missing");
    }

    if (!isFlagB) {
      throw Err("Machine not started because decision 'flagB' is missing");
    }

    if (!isFlagC) {
      throw Err("Machine not started because decision 'flagC' is missing");
    }

    if (!isFlagC1) {
      throw Err("Machine not started because decision 'flagC1' is missing");
    }

    if (!isFlagC2) {
      throw Err("Machine not started because decision 'flagC2' is missing");
    }


    // check actions
    if (!actionGoToA) {
      throw Err("Machine not started because action 'goToA' is missing");
    }

    if (!actionGoToB) {
      throw Err("Machine not started because action 'goToB' is missing");
    }

    if (!actionGoToF) {
      throw Err("Machine not started because action 'goToF' is missing");
    }

    if (!actionGoToG) {
      throw Err("Machine not started because action 'goToG' is missing");
    }


    // start the machine
    try {
      moveToState(state, [](bool){});
    }

    catch (std::exception& e) {
      throw std::runtime_error(e.what());
    }

  }

private:
  /**
   * Flag to indicate whether or not this class prints debugging messages.
   */
  /**
   * Current state.
   */
  State state;

  /**
   * Last action.
   */
  Action lastAction;

  /**
   * Last action error.
   */
  std::exception* lastActionError = NULL;

  /**
   * Actions
   */
  ActionExecutor actionGoToA;
  ActionExecutor actionGoToB;
  ActionExecutor actionGoToF;
  ActionExecutor actionGoToG;

  /**
   * Decisions
   */
  Decision isFlagA;
  Decision isFlagB;
  Decision isFlagC;
  Decision isFlagC1;
  Decision isFlagC2;

  /**
   * Transitions
   */
  Transition transitions[4] = {
    {kStateD, kActionGoToG, kStateG},
    {kStateG, kActionGoToA, kStateA},
    {kStateA, kActionGoToB, kStateB},
    {kStateE, kActionGoToF, kStateF},
  };

  StateMachine(State state) {
    this->state = state;
  }

  void  doAction(Action action, Completion completion) {
    log("doAction " + enumNameForAction(action));
    lastAction = action;

    ActionExecutor actionExec;
    switch (action) {
      case kActionGoToA:
      actionExec = actionGoToA;
      break;
      case kActionGoToB:
      actionExec = actionGoToB;
      break;
      case kActionGoToF:
      actionExec = actionGoToF;
      break;
      case kActionGoToG:
      actionExec = actionGoToG;
      break;
    }

    try {
      Transition transition = findTransition(action);
      if (!actionExec) {
        throw Err(kErrIdTransitionNotSet, state, action);
      }
      actionExec([&](bool success) {
        if (!success) {
          completion(false);
          return;
        }

        try {
          moveToState(std::get<2>(transition), completion);
        } catch (std::exception& e) {
          lastActionError = &e;
          completion(false);
        }

      }

      );
    } catch (std::exception& e) {
      lastActionError = &e;
      completion(false);
    }

  }

  Transition findTransition(Action action) {
    for (auto cand : transitions) {
      if (std::get<0>(cand) == state && std::get<1>(cand) == action) {
        return cand;
      }
    }

    throw Err(kErrIdImpossibleAction, state, action);
  }

  Action getOnePossibleAction(State state) {
    for (auto cand : transitions) {
      if (std::get<0>(cand) == state) {
        return std::get<1>(cand);
      }
    }

    throw Err(kErrIdNoActionsForState, state);
  }

  void moveToState(State state, Completion completion) {
    this->state = state;
    log("moveToState " + enumNameForState(state));

    switch (state) {
      case kStateADecision:
      if (isFlagA()) {
        moveToState(kStateA, completion);
      }

      else if (isFlagB()) {
        moveToState(kStateB, completion);
      }

      else if (isFlagC()) {
        moveToState(kStateCDecision, completion);
      }

      else {
        moveToState(kStateD, completion);
      }

      break;
      case kStateA:
      doAction(getOnePossibleAction(kStateA), completion);
      break;
      case kStateB:
      completion(true);
      break;
      case kStateCDecision:
      if (isFlagC1()) {
        moveToState(kStateE, completion);
      }

      else if (isFlagC2()) {
        moveToState(kStateF, completion);
      }

      else {
        moveToState(kStateG, completion);
      }

      break;
      case kStateD:
      completion(true);
      break;
      case kStateE:
      completion(true);
      break;
      case kStateF:
      completion(true);
      break;
      case kStateG:
      doAction(getOnePossibleAction(kStateG), completion);
      break;
    }

  }

  void log(std::string msg) {
    if (isLogEnabled) {
      std::cout << "StateMachine: " << msg << std::endl;
    }

  }

};
```

* and the usage file at `/tmp/MachineTest.cpp`:

```c++
#include "Machine.hpp"

class StateMachineTest {
public:
  void test() {
    StateMachine sm = StateMachine::create();
    sm.isLogEnabled = true;
    sm.setDecisionFlagA([&]() { /*TODO*/ return falsity(); });
    sm.setDecisionFlagB([&]() { /*TODO*/ return falsity(); });
    sm.setDecisionFlagC([&]() { /*TODO*/ return falsity(); });
    sm.setDecisionFlagC1([&]() { /*TODO*/ return falsity(); });
    sm.setDecisionFlagC2([&]() { /*TODO*/ return falsity(); });
    sm.setActionGoToA([&](StateMachine::Completion completion) { goToADemoEx(completion); });
    sm.setActionGoToB([&](StateMachine::Completion completion) { goToBDemoEx(completion); });
    sm.setActionGoToF([&](StateMachine::Completion completion) { goToFDemoEx(completion); });
    sm.setActionGoToG([&](StateMachine::Completion completion) { goToGDemoEx(completion); });
    sm.start();

    std::cout << "-- This returns a specific exception:" << std::endl;
    sm.doActionGoToF([&](bool success) {
      std::cout << "-- success:" << success << std::endl;
      std::cout << "-- error:" << sm.errorDescription() << std::endl;
    }

    );
    std::cout << "-- This moves through various states:" << std::endl;
    sm.doActionGoToG([&](bool success) {
      std::cout << "-- success:" << success << std::endl;
    }

    );
  }

private:
  void goToADemoEx(StateMachine::Completion completion) {
    // TODO: add logic for goToADemoEx
    std::cout << "-- StateMachineTest: goToADemoEx" << std::endl;
    completion(true);
  }

  void goToBDemoEx(StateMachine::Completion completion) {
    // TODO: add logic for goToBDemoEx
    std::cout << "-- StateMachineTest: goToBDemoEx" << std::endl;
    completion(true);
  }

  void goToFDemoEx(StateMachine::Completion completion) {
    // TODO: add logic for goToFDemoEx
    std::cout << "-- StateMachineTest: goToFDemoEx" << std::endl;
    completion(true);
  }

  void goToGDemoEx(StateMachine::Completion completion) {
    // TODO: add logic for goToGDemoEx
    std::cout << "-- StateMachineTest: goToGDemoEx" << std::endl;
    completion(true);
  }

  bool falsity() {
    return false;
  }

};

int main(int argc, char** argv) {
  StateMachineTest().test();
  return 0;
}
```

## TODO

1. Generate the Machine object automatically by parsing the BMPN diagram;

1. Beautify code (ex: join "else", "catch" lines);

1. Remove trailing white spaces;

1. Cleanup Common Lisp code;
