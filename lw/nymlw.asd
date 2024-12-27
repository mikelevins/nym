;;;; ***********************************************************************
;;;;
;;;; Name:          nymlw.asd
;;;; Project:       nym: an extensible name generator
;;;; Purpose:       system definition: Lispworks GUI for nym
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:cl-user)

(require :asdf)

#+lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf lw:*handle-warn-on-redefinition* :warn))

(asdf:defsystem #:nymlw
    :description "nym: a customizable name generator"
    :author "mikel evins <mikel@evins.net>"
    :license "Apache-2.0"
    :serial t
    :version (:read-file-form "../version.lisp")
    :depends-on (:alexandria :folio3+ :command-line-arguments)
    :components ((:module "src"
                          :serial t
                          :components ((:file "package")
                                       (:file "namer")
                                       #+lispworks (:file "ui"))))
  :build-operation asdf/bundle:program-op
  :build-pathname "nym"
  :entry-point "nym::main")


;;; (asdf:load-system :nymlw)
