(defpackage :rplanet/repository-interface
  (:use :cl)
  (:import-from :rplanet/utils
                :missing)
  (:export :*interface*
           :create-column
           :collect-column
           :find-column
           :create-task
           :collect-task
           :find-task
           :update-tasks))
(in-package :rplanet/repository-interface)

(defvar *interface*)

(defgeneric create-column (repository column))
(defgeneric collect-column (repository))
(defgeneric find-column (repository name))

(defmethod create-column :around (repository column)
  (call-next-method)
  column)

(defgeneric create-task (repository task))
(defgeneric collect-task (repository &key column-name))
(defgeneric find-task (repository &key column-name id))
(defgeneric update-tasks (repository tasks &key column-name))

(defmethod create-task :around (repository task)
  (call-next-method)
  task)

(defmethod update-tasks :before (repository tasks &key (column-name nil column-name-p))
  (declare (ignore column-name))
  (unless column-name-p
    (missing 'column-name)))

(defmethod update-tasks :around (repository tasks &key &allow-other-keys)
  (call-next-method)
  (values))
