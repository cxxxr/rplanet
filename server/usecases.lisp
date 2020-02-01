(defpackage :rplanet/usecases
  (:use :cl
        :rplanet/entities)
  (:local-nicknames (:i-repository :rplanet/repository-interface))
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
  (when (i-repository:find i-repository:*interface* 'column :name name)
    (error 'already-column-error :name name))
  (i-repository:create i-repository:*interface* (make-instance 'column :name name)))

(defun get-columns ()
  (i-repository:collect i-repository:*interface* 'column))

(defun add-task (&key (column-name (error "missing :column-name"))
                      (title (error "missing :title"))
                      (text ""))
  (unless (i-repository:find i-repository:*interface* 'column :name column-name)
    (error 'missing-column :name column-name))
  (let ((n (length (i-repository:collect i-repository:*interface* 'task)))) ; TODO: i-repository:count
    (let ((task (make-instance 'task
                               :column-name column-name
                               :title title
                               :text text
                               :priority (1+ n))))
      (i-repository:create i-repository:*interface*
                           task))))

(defun get-tasks ()
  (sort (copy-list (i-repository:collect i-repository:*interface* 'task))
        #'>
        :key #'task-priority))
