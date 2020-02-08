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
             (let ((object (rplanet/usecases:add-column name)))
               (ok (and (typep object 'rplanet/entities:column)
                        (equal (rplanet/entities:column-name object) name))))))
      (add "TODO")
      (add "In Progress"))
    (handler-case (progn
                    (rplanet/usecases:add-column "TODO")
                    (fail "Column does not already created"))
      (error (c)
        (ok (typep c 'rplanet/usecases:already-column-error))
        (ok (equal "TODO" (rplanet/usecases:already-column-error-name c)))
        (ok (string= "Column already created: TODO" (princ-to-string c)))))))

(deftest get-columns-tests
  (with
    (ok (= 0 (length (rplanet/usecases:get-columns))))
    (rplanet/usecases:add-column "TODO")
    (ok (= 1 (length (rplanet/usecases:get-columns))))
    (rplanet/usecases:add-column "In Progress")
    (ok (= 2 (length (rplanet/usecases:get-columns))))))

(deftest add-task
  (with
    (flet ((add (&rest args)
             (destructuring-bind (&key title column-name (text "")) args
               (let ((object (apply #'rplanet/usecases:add-task args)))
                 (ok (typep object 'rplanet/entities:task))
                 (ok (equal (rplanet/entities:task-title object) title))
                 (ok (equal (rplanet/entities:task-column-name object) column-name))
                 (ok (equal (rplanet/entities:task-text object) text))))))
      (rplanet/usecases:add-column "TODO")
      (add :column-name "TODO" :title "Hello World")
      (add :column-name "TODO" :title "second task")
      (handler-case (progn
                      (add :column-name "???" :title "second task")
                      (fail "Illegal task created"))
        (error (c)
          (ok (typep c 'rplanet/usecases:missing-column))
          (ok (equal "???" (rplanet/usecases:missing-column-name c)))
          (ok (string= "Missing column: ???" (princ-to-string c))))))))

(deftest get-tasks
  (with
    (rplanet/usecases:add-column "TODO")
    (ok (= 0 (length (rplanet/usecases:get-tasks))))
    (rplanet/usecases:add-task :column-name "TODO" :title "foo")
    (ok (= 1 (length (rplanet/usecases:get-tasks))))
    (rplanet/usecases:add-task :column-name "TODO" :title "bar")
    (ok (= 2 (length (rplanet/usecases:get-tasks))))
    (rplanet/usecases:add-task :column-name "TODO" :title "baz")
    (ok (= 3 (length (rplanet/usecases:get-tasks))))
    (ok (equal (mapcar #'rplanet/entities:task-title (rplanet/usecases:get-tasks))
               '("baz" "bar" "foo")))))

(deftest move-task
  (with
    (rplanet/usecases:add-column "TODO")
    (let* ((task1 (rplanet/usecases:add-task :column-name "TODO" :title "1"))
           (task2 (rplanet/usecases:add-task :column-name "TODO" :title "2"))
           (task3 (rplanet/usecases:add-task :column-name "TODO" :title "3"))
           (task4 (rplanet/usecases:add-task :column-name "TODO" :title "4")))
      (ok (equal (rplanet/usecases:get-tasks :column-name "TODO")
                 (list task4 task3 task2 task1)))
      (rplanet/usecases:move-task :from (rplanet/entities:task-id task1)
                                  :to (rplanet/entities:task-id task4))
      (ok (equal (rplanet/usecases:get-tasks :column-name "TODO")
                 (list task1 task4 task3 task2)))
      (rplanet/usecases:move-task :from (rplanet/entities:task-id task4)
                                  :to (rplanet/entities:task-id task2))
      (ok (equal (rplanet/usecases:get-tasks :column-name "TODO")
                 (list task1 task3 task2 task4))))))
