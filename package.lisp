;;;; package.lisp

(defpackage #:cl-statemachine-to-c++
  (:use #:cl #:pathname-utils)
  (:export :save-and-check-c++ :Machine))
