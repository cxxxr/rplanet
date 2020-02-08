(defpackage :rplanet/utils
  (:use :cl)
  (:export :missing))
(in-package :rplanet/utils)

(defun missing (arg)
  (error "missing :~A" arg))
