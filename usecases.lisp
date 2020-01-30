(defpackage :rplanet/presenters
  (:use :cl
        :rplanet/entities)
  (:local-nicknames (:i-repository :rplanet/repository-interface))
  (:export :add-column
           :add-task
           :get-tasks))
(in-package :rplanet/presenters)

(defun add-column (name)
  (i-repository:create i-repository:*interface* (make-instance 'column :name name)))

(defun add-task (&key (column-name (error "missing :column-name"))
                      (title (error "missing :title"))
                      (text ""))
  (i-repository:create i-repository:*interface*
                       (make-instance 'task
                                      :column-name column-name
                                      :title title
                                      :text text)))

(defun get-tasks ()
  (i-repository:collect i-repository:*interface* 'task))
