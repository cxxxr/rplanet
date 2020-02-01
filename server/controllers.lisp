(defpackage :rplanet/controllers
  (:use :cl)
  (:export :post-column
           :get-columns
           :post-task
           :get-tasks))
(in-package :rplanet/controllers)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun lisp-variable-to-request-parameter-name (name)
    (format nil "~{~(~A~)~^_~}" (uiop:split-string (string name) :separator "-"))))

(defmacro with-params (bindings params &body body)
  (alexandria:once-only (params)
    `(let ,(mapcar (lambda (b)
                     (destructuring-bind
                           (var &key (key (lisp-variable-to-request-parameter-name var))
                                default)
                         (alexandria:ensure-list b)
                       `(,var (assoc-utils:aget ,params ,key ,default))))
                   bindings)
       ,@body)))

(defun alist-to-jso (alist)
  (st-json::make-jso :alist alist))

(defun render-json (jso &key (status 200))
  (list status
        `(:content-type "application/json")
        (list (st-json:write-json-to-string jso))))

(defun post-column (params)
  (with-params (name) params
    (let ((column (rplanet/usecases:add-column name)))
      (render-json (st-json:jso "name" (rplanet/entities:column-name column))))))

(defun get-columns (params)
  (declare (ignore params))
  (let ((columns (rplanet/usecases:get-columns)))
    (render-json
     (st-json:jso "children" (mapcar (lambda (column)
                                       (st-json:jso "name"
                                                    (rplanet/entities:column-name column)))
                                     columns)))))

(defun post-task (params)
  (with-params (column-name
                title
                (text :default ""))
      params
    (let ((task (rplanet/usecases:add-task :column-name column-name
                                           :title title
                                           :text text)))
      (render-json (st-json:jso "column_name" (rplanet/entities:task-column-name task)
                                "title" (rplanet/entities:task-title task)
                                "text" (rplanet/entities:task-text task))))))

(defun get-tasks (params)
  (declare (ignore params))
  (render-json
   (st-json:jso "children"
                (mapcar (lambda (task)
                          (st-json:jso "title" (rplanet/entities:task-title task)
                                       "text" (rplanet/entities:task-text task)
                                       "column_name" (rplanet/entities:task-column-name task)))
                        (rplanet/usecases:get-tasks)))))
