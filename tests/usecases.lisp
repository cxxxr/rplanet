(defpackage :rplanet-tests/usecases
  (:use :cl :rove))
(in-package :rplanet-tests/usecases)

(defmacro with (&body body)
  `(let ((rplanet/repository-interface:*interface*
           (make-instance 'rplanet/repository:repository))
         (rplanet/repository::*db* (rplanet/repository:make-db)))
     ,@body))

(deftest add-column-tests
  (with
    (flet ((add (name)
             (let ((object (rplanet/presenters:add-column name)))
               (ok (and (typep object 'rplanet/entities:column)
                        (equal (rplanet/entities:column-name object) name))))))
      (add "TODO")
      (add "In Progress"))
    (handler-case (progn
                    (rplanet/presenters:add-column "TODO")
                    (fail "Column does not already created"))
      (error (c)
        (ok (typep c 'rplanet/presenters:already-column-error))
        (ok (equal "TODO" (rplanet/presenters:already-column-error-name c)))
        (ok (string= "Column already created: TODO" (princ-to-string c)))))))

(deftest get-columns-tests
  (with
    (ok (= 0 (length (rplanet/presenters:get-columns))))
    (rplanet/presenters:add-column "TODO")
    (ok (= 1 (length (rplanet/presenters:get-columns))))
    (rplanet/presenters:add-column "In Progress")
    (ok (= 2 (length (rplanet/presenters:get-columns))))))

(deftest add-task
  (with
    (flet ((add (&rest args)
             (destructuring-bind (&key title column-name (text "")) args
               (let ((object (apply #'rplanet/presenters:add-task args)))
                 (ok (and (typep object 'rplanet/entities:task)
                          (equal (rplanet/entities:task-title object) title)
                          (equal (rplanet/entities:task-column-name object) column-name)
                          (equal (rplanet/entities:task-text object) text)))))))
      (rplanet/presenters:add-column "TODO")
      (add :column-name "TODO" :title "Hello World")
      (add :column-name "TODO" :title "second task")
      (handler-case (progn
                      (add :column-name "???" :title "second task")
                      (fail "Illegal task created"))
        (error (c)
          (ok (typep c 'rplanet/presenters:missing-column))
          (ok (equal "???" (rplanet/presenters:missing-column-name c)))
          (ok (string= "Missing column: ???" (princ-to-string c))))))))

(deftest get-tasks
  (with
    (rplanet/presenters:add-column "TODO")
    (ok (= 0 (length (rplanet/presenters:get-tasks))))
    (rplanet/presenters:add-task :column-name "TODO" :title "foo")
    (ok (= 1 (length (rplanet/presenters:get-tasks))))
    (rplanet/presenters:add-task :column-name "TODO" :title "bar")
    (ok (= 2 (length (rplanet/presenters:get-tasks))))))
