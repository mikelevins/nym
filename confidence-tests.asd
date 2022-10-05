;;;; ***********************************************************************
;;;;
;;;; Name:          confidene-tests.asd
;;;; Project:       nym 
;;;; Purpose:       confidence tests of nym
;;;; Author:        mikel evins
;;;; Copyright:     2022 by mikel evins
;;;;
;;;; ***********************************************************************

(asdf:defsystem :confidence-tests
  :serial t
  :description "confidence tests"
  :author "mikel evins <mevins@me.com>"
  :license "Lisp Lesser GNU Public License"
  :depends-on (:nym :lift)
  :components ((:module "confidence-tests"
                        :serial t
                        :components ((:file "tests"))))
  :perform (asdf:test-op (o c)
             (symbol-call :confidence-tests :run-tests)))

;;; (asdf:load-system :confidence-tests)
;;; (asdf:test-system :confidence-tests)
