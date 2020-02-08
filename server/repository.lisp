(defpackage :rplanet/repository
  (:use :cl :rplanet/entities :rplanet/utils)
  (:local-nicknames (:i-repository :rplanet/repository-interface))
  (:export :make-db
           :repository))
(in-package :rplanet/repository)

(defun make-db ()
  '())

(defvar *db* (make-db))

(defun find-column (column-name)
  (flet ((key (x) (column-name (car x))))
    (find column-name *db*
          :test #'string=
          :key #'key)))

(defclass repository () ())

(defmethod i-repository:create-column ((repository repository) column)
  (push-end (cons column nil) *db*))

(defmethod i-repository:collect-column ((repository repository))
  (mapcar #'car *db*))

(defmethod i-repository:find-column ((repository repository) name)
  (car (find-column name)))

(defmethod i-repository:create-task ((repository repository) task)
  (let ((column (find-column (task-column-name task))))
    (push task (cdr column))))

(defmethod i-repository:collect-task ((repository repository) &key column-name)
  (if column-name
      (cdr (find-column column-name))
      (loop :for (column . tasks) :in *db*
            :append tasks)))

(defmethod i-repository:find-task ((repository repository) &key column-name id)
  (find-if (lambda (task)
             (or (null id)
                 (equal (task-id task) id)))
           (i-repository:collect-task repository :column-name column-name)))

(defmethod i-repository:update-tasks ((repository repository) tasks &key column-name)
  (setf (cdr (find-column column-name))
        tasks))
