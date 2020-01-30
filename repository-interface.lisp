(defpackage :rplanet/repository-interface
  (:use :cl)
  (:export :*interface*
           :create
           :collect))
(in-package :rplanet/repository-interface)

(defvar *interface*)

(defgeneric create (repository entity))
(defgeneric collect (repository-name entity &rest args))
