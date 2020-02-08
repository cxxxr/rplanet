(defpackage :rplanet/usecases
  (:use :cl
        :rplanet/entities)
  (:local-nicknames (:i-repository :rplanet/repository-interface))
  (:import-from :rplanet/utils
                :missing)
  (:export :rplanet-error
           :already-column-error
           :already-column-error-name
           :missing-column
           :missing-column-name
           :add-column
           :get-columns
           :add-task
           :get-tasks))
(in-package :rplanet/usecases)

(define-condition rplanet-error (simple-error)
  ())

(define-condition already-column-error (rplanet-error)
  ((name
    :initarg :name
    :reader already-column-error-name))
  (:report (lambda (condition stream)
             (format stream
                     "Column already created: ~A"
                     (already-column-error-name condition)))))

(define-condition missing-column (rplanet-error)
  ((name
    :initarg :name
    :reader missing-column-name))
  (:report (lambda (condition stream)
             (format stream
                     "Missing column: ~A"
                     (missing-column-name condition)))))

(defun add-column (name)
  (when (i-repository:find-column i-repository:*interface* name)
    (error 'already-column-error :name name))
  (i-repository:create-column i-repository:*interface*
                              (make-instance 'column :name name)))

(defun get-columns ()
  (i-repository:collect-column i-repository:*interface*))

(defun add-task (&key (column-name (missing 'column-name))
                      (title (missing 'title))
                      (text ""))
  (unless (i-repository:find-column i-repository:*interface* column-name)
    (error 'missing-column :name column-name))
  (let ((task (make-instance 'task
                             :column-name column-name
                             :title title
                             :text text)))
    (i-repository:create-task i-repository:*interface* task)))

(defun get-tasks ()
  (i-repository:collect-task i-repository:*interface*))
