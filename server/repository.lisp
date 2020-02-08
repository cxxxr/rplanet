(defpackage :rplanet/repository
  (:use :cl :rplanet/entities :rplanet/utils)
  (:local-nicknames (:i-repository :rplanet/repository-interface))
  (:export :make-db
           :repository))
(in-package :rplanet/repository)

(defun make-db ()
  '())

(defvar *db* (make-db))

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
  (let ((column
          (find (task-column-name task)
                *db*
                :test #'string=
                :key (alexandria:compose #'column-name #'car))))
    (push task (cdr column))))

(defmethod i-repository:collect-task ((repository repository) &key column-name)
  (if column-name
      (cdr (find column-name
                 *db*
                 :test #'string=
                 :key (alexandria:compose #'column-name #'car)))
      (loop :for (column . tasks) :in *db*
            :append tasks)))

(defmethod i-repository:find-task ((repository repository) &key column-name id)
  (find-if (lambda (task)
             (or (null id)
                 (equal (task-id task) id)))
           (i-repository:collect-task repository :column-name column-name)))

(defmethod i-repository:update-tasks ((repository repository) tasks &key column-name)
  (setf (cdr (find column-name
                   *db*
                   :test #'string=
                   :key (alexandria:compose #'column-name #'car)))
        tasks))
