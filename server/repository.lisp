(defpackage :rplanet/repository
  (:use :cl :rplanet/entities)
  (:local-nicknames (:i-repository :rplanet/repository-interface))
  (:export :make-db
           :repository))
(in-package :rplanet/repository)

(defun make-db ()
  (make-hash-table))

(defvar *db* (make-db))

(defclass repository () ())

(defmethod i-repository:create ((repository repository) entity)
  (push entity (gethash (type-of entity) *db* nil)))

(defmethod i-repository:collect ((repository repository) entity-name &rest args)
  (declare (ignore args))
  (gethash entity-name *db*))

(defmethod i-repository:find ((repository repository) (entity-name (eql 'column)) &rest args)
  (destructuring-bind (&key name) args
    (find name (gethash entity-name *db*)
          :test #'string=
          :key #'column-name)))
