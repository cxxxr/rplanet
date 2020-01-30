(defpackage :rplanet/repository-interface
  (:use :cl)
  (:shadow :find)
  (:export :*interface*
           :create
           :collect
           :find))
(in-package :rplanet/repository-interface)

(defvar *interface*)

(defgeneric create (repository entity))
(defgeneric collect (repository entity-name &rest args))
(defgeneric find (repository entity-name &rest args))

(defmethod create :around (repository entity)
  (call-next-method)
  entity)
