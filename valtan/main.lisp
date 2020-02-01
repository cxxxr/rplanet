;; -*- mode: valtan -*-

(ffi:require js:react "react")
(ffi:require js:react-dom "react-dom")
(ffi:require js:-modal "react-modal")

(defpackage :rplanet
  (:use :cl :valtan.react-utilities))
(in-package :rplanet)

(setf (symbol-function '<modal>) js:-modal)

(defun alist-to-object (alist)
  (apply #'ffi::%object
         (mapcan (lambda (elt)
                   (list (car elt)
                         (ffi:cl->js (cdr elt))))
                 alist)))

(defun headers-to-object (headers)
  (alist-to-object
   (mapcar (lambda (header)
             (cons (cond ((keywordp (car header))
                          (string-downcase (car header)))
                         (t
                          (car header)))
                   (cdr header)))
           headers)))

(defun request (url &key method (headers '((:content-type . "application/json"))) then content)
  (let ((body (if (and content
                       (equal (cdr (assoc :content-type headers))
                              "application/json")
                       (consp content))
                  ((ffi:ref "JSON" :stringify)
                   (alist-to-object content))
                  nil)))
    (let ((option
            `((:method . ,(ffi:cl->js (string method)))
              (:headers . ,(headers-to-object headers))
              (:then . ,then)
              ,@(when body `((:body . ,body))))))
      (js:fetch (ffi:cl->js url)
                (alist-to-object option)))))

(defmacro %then (form arg)
  `((ffi:ref ,form :then) ,arg))

(eval-when (:compile-toplevel)
  (defun expand-then-forms (form args)
    (if (null args)
        form
        (expand-then-forms `(%then ,form ,(first args))
                           (rest args)))))

(defmacro then (form &rest args)
  (expand-then-forms form args))

(defun map-with-index (function sequence)
  (let ((i -1))
    (map (type-of sequence)
         (lambda (elt)
           (funcall function elt (incf i)))
         sequence)))

(define-react-component <task-input> (on-input)
  (with-state ((value set-value #j""))
    (flet ((handle-submit (e)
             ((ffi:ref e :prevent-default))
             (funcall on-input (ffi:js->cl value)))
           (handle-change (e)
             (set-value (ffi:ref e :target :value))))
      (jsx (:form (:on-submit #'handle-submit)
            (:input (:on-change #'handle-change)))))))

(defparameter +column-width+ 400)
(defparameter +column-padding+ 10)

(defun fetch-data (set-tasks set-columns)
  (then (request "/columns" :method :get)
        (lambda (response)
          ((ffi:ref response :json)))
        (lambda (response)
          (let ((columns (ffi:js->cl (ffi:ref response :children))))
            (funcall set-columns columns))))
  (then (request "/tasks" :method :get)
        (lambda (response)
          ((ffi:ref response :json)))
        (lambda (response)
          (let ((tasks (ffi:js->cl (ffi:ref response :children))))
            (funcall set-tasks tasks)))))

(define-react-component <column> (column tasks on-add-task)
  (jsx (:article (:style (ffi:object :background #j"#efefef"
                                     :padding #j(format nil "~Apx 0px ~Apx ~Apx"
                                                        +column-padding+
                                                        +column-padding+
                                                        +column-padding+)
                                     :margin #j"10px 10px 10px 0px"
                                     :width (+ +column-width+ +column-padding+)))
        (let* ((column-name (ffi:ref column :name))
               (tasks (remove column-name tasks
                              :test-not #'equal
                              :key (lambda (task) (ffi:ref task :column_name)))))
          (jsx (:div ()
                (:div ()
                 (:h1 () column-name)
                 (:button (:on-click (lambda (e)
                                       (declare (ignore e))
                                       (funcall on-add-task column-name)))
                  "+"))
                (map-with-index (lambda (item i)
                                  (jsx (:article (:key i
                                                  :style (ffi:object :background #j"#efefef"
                                                                     :padding #j(format nil "~Apx ~Apx 0px 0px"
                                                                                        +column-padding+
                                                                                        +column-padding+)))
                                        (:div (:style (ffi:object
                                                       :width +column-width+
                                                       :height 200
                                                       :background #j"white"))
                                         (:span ()
                                          (ffi:ref item :title))))))
                                tasks)))))))

(define-react-component <task-adding-modal> (enable on-input)
  (jsx (<modal> (:is-open (if enable #j:true #j:false))
                (<task-input> (:on-input on-input)))))

(define-react-component <app> ()
  (with-state ((tasks set-tasks #())
               (columns set-columns #())
               (modal-state set-modal-state nil)
               (require-update-p set-require-update-p t))
    (js:react.use-effect
     (lambda ()
       (set-require-update-p nil)
       (fetch-data #'set-tasks #'set-columns)
       ;; XXX: useEffectの中では関数以外を返してはいけないので、何もしないfinallize関数を返す
       (lambda ()))
     (ffi:array require-update-p))
    (jsx (:div ()
          (<task-adding-modal> (:enable modal-state
                                :on-input (lambda (text)
                                            (set-modal-state nil)
                                            (then (request "/tasks"
                                                           :method :post
                                                           :content `((:title . ,text)
                                                                      (:column_name . ,modal-state)))
                                                  (lambda (response)
                                                    ((ffi:ref response :json)))
                                                  (lambda (response)
                                                    (set-require-update-p t))))))
          (:div (:style (ffi:object :display #j"flex" :flex-direction #j"row"))
           (map-with-index (lambda (column column-index)
                             (jsx (<column> (:column column
                                             :key column-index
                                             :tasks tasks
                                             :on-add-task (lambda (column-name)
                                                            (set-modal-state column-name))))))
                           columns))))))

(setup #'<app> "root")

#+(or)
(valtan.remote-eval:connect
 (lambda ()
   (setup #'<app> "root")))
