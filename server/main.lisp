(defpackage :rplanet/main
  (:use :cl)
  (:export :start
           :stop))
(in-package :rplanet/main)

(setq rplanet/repository-interface:*interface* (make-instance 'rplanet/repository:repository))

(defclass app (ningle:app) ())

(defmethod lack.component:to-app ((app app))
  (lack.builder:builder
   (:static
    :path (lambda (path)
            (if (ppcre:scan "^(?:/assets/|/robot\\.txt$|/favicon\\.ico$)" path)
                path
                nil))
    :root (asdf:system-relative-pathname :rplanet #P"public/"))
   (call-next-method)))

(defvar *app* (make-instance 'app))
(defvar *handler* nil)

(setf (ningle:route *app* "/" :method :GET) (asdf:system-relative-pathname :rplanet #p"public/assets/index.html"))
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
