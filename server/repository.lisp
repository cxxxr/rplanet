(defpackage :rplanet/repository
  (:use :cl :rplanet/entities)
  (:local-nicknames (:i-repository :rplanet/repository-interface))
  (:export :make-db
           :repository))
(in-package :rplanet/repository)

(defun make-db ()
  '())

(defvar *db* (make-db))

(defmacro push-end (object place)
  `(alexandria:nconcf ,place (list ,object)))

(defclass repository () ())

(defmethod i-repository:create-column ((repository repository) column)
  (push-end (cons column nil) *db*))

(defmethod i-repository:collect-column ((repository repository))
  (mapcar #'car *db*))

(defmethod i-repository:find-column ((repository repository) name)
  (car (find name *db*
             :test #'string=
             :key (alexandria:compose #'column-name #'car))))

(defmethod i-repository:create-task ((repository repository) task)
  (push task
        (cdr (find (task-column-name task)
                   *db*
                   :key (alexandria:compose #'column-name #'car)))))

(defmethod i-repository:collect-task ((repository repository) &key column-name)
  (if column-name
      (mapcar #'cdr
              (find column-name
                    *db*
                    :key (alexandria:compose #'column-name #'car)))
      (loop :for (column . tasks) :in *db*
            :append tasks)))

(defmethod i-repository:find-task ((repository repository) &key column-name id)
  (find-if (lambda (task)
             (or (null id)
                 (equal (task-id task) id)))
           (i-repository:collect-task repository :column-name column-name)))

(defmethod i-repository:update-tasks ((repository repository) tasks &key column-name)
  (setf (gethash column-name *db*) tasks))

