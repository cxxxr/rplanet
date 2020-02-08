(defpackage :rplanet/utils
  (:use :cl)
  (:export :missing
           :push-end))
(in-package :rplanet/utils)

(defun missing (arg)
  (error "missing :~A" arg))

(defmacro push-end (object place)
  `(alexandria:nconcf ,place (list ,object)))
