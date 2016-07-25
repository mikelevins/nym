;;;; nym.asd

(asdf:defsystem #:nym
  :description "Describe nym here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "travesty")
                                     (:file "utils")))))


;;; (asdf:load-system :nym)
