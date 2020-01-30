(defpackage :rplanet/controllers
  (:use :cl)
  (:export :post-column
           :get-columns
           :post-task
           :get-tasks))
(in-package :rplanet/controllers)

(trivia:defpattern alist (&rest args)
  `(and ,@(mapcar (trivia.level0:lambda-match0
                    ((cons key pattern)
                     `(assoc ,key ,pattern :test #'equal)))
                  args)))

(defun render-json (alist &key (status 200))
  (list status
        `(:content-type "application/json")
        (list (jojo:to-json alist :from :alist))))

(defun post-column (params)
  (trivia:ematch params
    ((alist ("name" . name))
     (let ((column (rplanet/presenters:add-column name)))
       (render-json `(("name" . ,(rplanet/entities:column-name column))))))))

(defun get-columns (params)
  (declare (ignore params))
  (let ((columns (rplanet/presenters:get-columns)))
    (render-json (acons "children"
                        (map 'vector
                             (lambda (column)
                               (acons "column" (rplanet/entities:column-name column) nil))
                             columns)
                        nil))))

(defun post-task (params)
  (trivia:ematch params
    ((alist ("column_name" . column-name)
            ("title" . title)
            ("text" . text))
     (let ((task (rplanet/presenters:add-task :column-name column-name
                                              :title title
                                              :text text)))
       (render-json `(("column_name" . ,(rplanet/entities:task-column-name task))
                      ("title" . ,(rplanet/entities:task-title task))
                      ("text" . ,(rplanet/entities:task-text task))))))))

(defun get-tasks (params)
  (declare (ignore params))
  (rplanet/presenters:get-tasks))
