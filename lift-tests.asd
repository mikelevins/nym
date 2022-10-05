;;;; ***********************************************************************
;;;;
;;;; Name:          lift-tests.asd
;;;; Project:       nym 
;;;; Purpose:       lift tests of nym
;;;; Author:        mikel evins
;;;; Copyright:     2022 by mikel evins
;;;;
;;;; ***********************************************************************

(asdf:defsystem :lift-tests
  :serial t
  :description "lift tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:nym :lift)
  :components ((:module "lift-tests"
                        :serial t
                        :components ((:file "tests"))))
  :perform (asdf:test-op (o c)
             (symbol-call :lift-tests :run-tests)
             (symbol-call :lift :describe-test-result
                          (symbol-value (find-symbol* :*test-result* :lift)) t)))

;;; (asdf:load-system :lift-tests)
;;; (asdf:test-system :lift-tests)
