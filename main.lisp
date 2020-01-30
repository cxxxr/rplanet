(defpackage :rplanet/main
  (:use :cl))
(in-package :rplanet/main)

(setq rplanet/repository-interface:*interface* (make-instance 'rplanet/repository:repository))

(defvar *app* (make-instance 'ningle:app))
(defvar *handler* nil)

(setf (ningle:route *app* "/columns" :method :POST) 'rplanet/controllers:post-column)
(setf (ningle:route *app* "/columns" :method :GET) 'rplanet/controllers:get-columns)
(setf (ningle:route *app* "/tasks" :method :POST) 'rplanet/controllers:post-task)
(setf (ningle:route *app* "/tasks" :method :GET) 'rplanet/controllers:get-tasks)

(defun start ()
  (unless *handler*
    (setq *handler* (clack:clackup *app*))))

(defun stop ()
  (when *handler*
    (clack:stop *handler*)
    (setq *handler* nil)))
