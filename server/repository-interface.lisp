(defpackage :rplanet/repository-interface
  (:use :cl)
  (:export :*interface*
           :create-column
           :collect-column
           :find-column
           :create-task
           :collect-task
           :find-task))
(in-package :rplanet/repository-interface)

(defvar *interface*)

(defgeneric create-column (repository column))
(defgeneric collect-column (repository))
(defgeneric find-column (repository name))

(defmethod create-column :around (repository column)
  (call-next-method)
  column)

(defgeneric create-task (repository task))
(defgeneric collect-task (repository))
(defgeneric find-task (repository &key column-name id))

(defmethod create-task :around (repository task)
  (call-next-method)
  task)
