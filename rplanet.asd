(defsystem "rplanet"
  :serial t
  :components ((:file "entities")
               (:file "repository-interface")
               (:file "usecases")
               (:file "repository")
               (:file "main")))

(defsystem "rplanet-tests"
  :depends-on ("rove" "rplanet")
  :serial t
  :pathname "tests"
  :components ((:file "usecases")))
