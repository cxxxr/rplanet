(defsystem "rplanet"
  :depends-on ("ningle" "trivia" "clack" "jonathan")
  :serial t
  :components ((:file "entities")
               (:file "repository-interface")
               (:file "usecases")
               (:file "repository")
               (:file "controllers")
               (:file "main")))

(defsystem "rplanet-tests"
  :depends-on ("rove" "rplanet")
  :serial t
  :pathname "tests"
  :components ((:file "usecases")))
