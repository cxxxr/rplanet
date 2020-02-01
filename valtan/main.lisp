(ffi:require js:react "react")
(ffi:require js:react-dom "react-dom")

(defpackage :rplanet
  (:use :cl :valtan.react-utilities))
(in-package :rplanet)

(define-react-component <app> ()
  (tag :h1 () "hello world"))

(setup #'<app> "root")
