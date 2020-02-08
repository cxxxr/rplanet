(defpackage :rplanet/entities
  (:use :cl)
  (:import-from :rplanet/utils
                :missing)
  (:export :column
           :column-name
           :task
           :task-id
           :task-title
           :task-text
           :task-column-name))
(in-package :rplanet/entities)

(defclass column ()
  ((name
    :initarg :name
    :reader column-name)))

(defvar *task-id* 0)

(defclass task ()
  ((id
    :reader task-id
    :initform (incf *task-id*))
   (title
    :initarg :title
    :accessor task-title)
   (text
    :initarg :text
    :accessor task-text)
   (column-name
    :initarg :column-name
    :accessor task-column-name)))
