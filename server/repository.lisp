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

(defmacro push-end (object place)
  `(alexandria:nconcf ,place (list ,object)))

(defmethod i-repository:create-column ((repository repository) column)
  (push-end column (gethash 'column *db* nil)))

(defmethod i-repository:collect-column ((repository repository))
  (values (gethash 'column *db* nil)))

(defmethod i-repository:find-column ((repository repository) name)
  (find name (gethash 'column *db*)
        :test #'string=
        :key #'column-name))

(defmethod i-repository:create-task ((repository repository) task)
  (push task (gethash 'task *db* nil)))

(defmethod i-repository:collect-task ((repository repository))
  (values (gethash 'task *db* nil)))

(defmethod i-repository:find-task ((repository repository) &key (column-name nil column-name-p) (id nil id-p))
  (find-if (lambda (task)
             (and (or (not column-name-p)
                      (equal (task-column-name task) column-name))
                  (or (not id-p)
                      (equal (task-id task) id))))
           (gethash 'task *db*)))
