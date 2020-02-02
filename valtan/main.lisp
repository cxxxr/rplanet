;; -*- mode: valtan -*-

(ffi:require js:react "react")
(ffi:require js:react-dom "react-dom")
(ffi:require js:-modal "react-modal")
(ffi:require js:react-dnd "react-dnd")
(ffi:require js:-backend "react-dnd-html5-backend")

(defpackage :rplanet
  (:use :cl :valtan.react-utilities))
(in-package :rplanet)

(setf (symbol-function '<react-modal>) js:-modal)
(setf (symbol-function '<react-dnd.use-drag>) js:react-dnd.use-drag)
(setf (symbol-function '<dnd-provider>) js:react-dnd.-dnd-provider)
(setf (symbol-function '<backend>) js:-backend.default)

(js:-modal.set-app-element #j"#root")

(defmacro fn (&body body)
  `(lambda (&rest args)
     ,@body))

(eval-when (:compile-toplevel)
  (defun gen-match-array (seq value op)
    (let ((g-value (gensym)))
      (acons g-value (list value)
             (let ((acc nil)
                   (index 0))
               (map nil (lambda (pat)
                          (let ((tmp (gensym)))
                            (push `(,tmp (,op ,g-value ,index)) acc)
                            (dolist (bind (gen-bind-match pat tmp))
                              (push bind acc)))
                          (incf index))
                    seq)
               (nreverse acc)))))
  (defun gen-bind-match (pattern value)
    (cond ((and (symbolp pattern)
                (not (keywordp pattern)))
           `((,pattern ,value)))
          ((and (consp pattern)
                (eq 'ffi:object (first pattern)))
           (when (rest pattern)
             (let ((tmp (gensym)))
               (acons tmp (list value)
                      (loop :for var :in (rest pattern)
                            :collect `(,var (ffi:ref ,value ,(intern (string var) :keyword))))))))
          ((and (consp pattern)
                (eq 'ffi:array (first pattern)))
           (gen-match-array (rest pattern) value 'ffi:aget))
          ((and (vectorp pattern)
                (not (stringp pattern)))
           (gen-match-array pattern value 'aref))
          (t
           (error "unexpected pattern: ~S" pattern)))))

(defmacro bind (pattern value &body body)
  `(let* ,(gen-bind-match pattern value)
     ,@body))

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


(defparameter +column-width+ 400)
(defparameter +column-padding+ 10)

(defparameter +task+ #j"task")

(define-react-component <task> (item)
  (jsx (:article (:style (ffi:object :background #j"#efefef"
                                     :padding #j(format nil "~Apx ~Apx 0px 0px"
                                                        +column-padding+
                                                        +column-padding+)))
        (:div (:style (ffi:object
                       :width +column-width+
                       :height 200
                       :background #j"white"))
         (:span ()
          (ffi:ref item :title))))))

(define-react-component <draggable-task> (item)
  (bind (ffi:array (ffi:object is-dragging) drag-ref)
      (<react-dnd.use-drag>
       (ffi:object :item (ffi:object :type +task+)
                   :collect (lambda (monitor)
                              (ffi:object :is-dragging ((ffi:ref monitor :is-dragging))))))
    (jsx (:div (:ref drag-ref)
          (<task> (:item item))))))

(define-react-component <tasklist> (column tasks on-add-task)
  (jsx (:article (:style (ffi:object :background #j"#efefef"
                                     :padding (ffi:cl->js
                                               (format nil "~Apx 0px ~Apx ~Apx"
                                                       +column-padding+
                                                       +column-padding+
                                                       +column-padding+))
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
                                  (jsx (<draggable-task> (:key i :item item))))
                                tasks)))))))

(define-react-component <task-input> (on-input)
  (with-state ((value set-value #j""))
    (flet ((handle-submit (e)
             ((ffi:ref e :prevent-default))
             (funcall on-input (ffi:js->cl value)))
           (handle-change (e)
             (set-value (ffi:ref e :target :value))))
      (jsx (:form (:on-submit #'handle-submit)
            (:input (:on-change #'handle-change)))))))

(define-react-component <modal> (enable on-input)
  (jsx (<react-modal> (:is-open (if enable #j:true #j:false))
                      (<task-input> (:on-input on-input)))))

(defstruct modal-state
  type
  value
  on-input)

(defun handle-task-input (modal-state text on-responsed)
  (then (request "/tasks"
                 :method :post
                 :content `((:title . ,text)
                            (:column_name . ,(modal-state-value modal-state))))
        (lambda (response)
          ((ffi:ref response :json)))
        (lambda (response)
          (funcall on-responsed response))))

(defun handle-column-input (modal-state text on-responsed)
  (then (request "/columns"
                 :method :post
                 :content `((:name . ,text)))
        (lambda (response)
          ((ffi:ref response :json)))
        (lambda (response)
          (funcall on-responsed response))))

(define-react-component <tasktable> (columns tasks on-add-task)
  (jsx (:div (:style (ffi:object :display #j"flex" :flex-direction #j"row"))
        (map-with-index (lambda (column column-index)
                          (jsx (<tasklist> (:column column
                                            :key column-index
                                            :tasks tasks
                                            :on-add-task (lambda (column-name)
                                                           (funcall on-add-task
                                                                    (make-modal-state
                                                                     :type :add-task
                                                                     :value column-name
                                                                     :on-input #'handle-task-input)))))))
                        columns))))

(define-react-component <add-column-button> (on-add-column)
  (jsx (:button (:on-click (lambda (e)
                             (declare (ignore e))
                             (funcall on-add-column
                                      (make-modal-state :type :add-column
                                                        :on-input #'handle-column-input))))
        "Add column")))

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

(define-react-component <kanban> ()
  (with-state ((tasks set-tasks #())
               (columns set-columns #())
               (modal-state set-modal-state nil)
               (require-update-p set-require-update-p t))
    (js:react.use-effect
     (lambda ()
       (set-require-update-p nil)
       (fetch-data #'set-tasks #'set-columns)
       #j:undefined)
     (ffi:array require-update-p))
    (jsx (:div ()
          (<modal> (:enable modal-state
                    :on-input (lambda (text)
                                (set-modal-state nil)
                                (when (modal-state-on-input modal-state)
                                  (funcall (modal-state-on-input modal-state)
                                           modal-state
                                           text
                                           (fn (set-require-update-p t)))))))
          (:div (:style (ffi:object :display #j"flex" :flex-direction #j"row"))
           (<tasktable> (:columns columns
                         :tasks tasks
                         :on-add-task #'set-modal-state))
           (<add-column-button> (:on-add-column #'set-modal-state)))))))

(define-react-component <app> ()
  (jsx
   (<dnd-provider> (:backend #'<backend>)
                   (<kanban> ()))))

(setup #'<app> "root")

(valtan.remote-eval:connect
 (lambda ()
   (setup #'<app> "root")))
