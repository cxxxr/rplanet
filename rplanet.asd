(defsystem "rplanet"
  :depends-on ("ningle" "clack" "st-json" "assoc-utils")
  :serial t
  :pathname "server"
  :components ((:file "entities")
               (:file "repository-interface")
               (:file "usecases")
               (:file "repository")
               (:file "controllers")
               (:file "main")))

(defsystem "rplanet-tests"
  :depends-on ("rove" "rplanet")
  :serial t
  :pathname "server/tests"
  :components ((:file "usecases")))
