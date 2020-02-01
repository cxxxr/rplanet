(defpackage :rplanet/entities
  (:use :cl)
  (:export :column
           :column-name
           :task
           :task-title
           :task-text
           :task-column-name
           :task-priority))
(in-package :rplanet/entities)

(defclass column ()
  ((name
    :initarg :name
    :reader column-name)))

(defclass task ()
  ((title
    :initarg :title
    :accessor task-title)
   (text
    :initarg :text
    :accessor task-text)
   (column-name
    :initarg :column-name
    :accessor task-column-name)
   (priority
    :initarg :priority
    :initform (error "Missing :priority")
    :accessor task-priority)))
