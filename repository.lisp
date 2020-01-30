(defpackage :rplanet/repository
  (:use :cl :rplanet/repository-interface)
  (:export :repository))
(in-package :rplanet/repository)

(defvar *db* (make-hash-table))

(defclass repository () ())

(defmethod create ((repository repository) entity)
  (push entity (gethash (type-of entity) *db* nil)))

(defmethod collect ((repository repository) entity-name &rest args)
  (declare (ignore args))
  (gethash entity-name *db*))
