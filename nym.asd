;;;; ***********************************************************************
;;;;
;;;; Name:          nym.asd
;;;; Project:       nym: an extensible name generator
;;;; Purpose:       system definitions
;;;; Author:        mikel evins
<<<<<<< HEAD
;;;; Copyright:     2019 by mikel evins
=======
;;;; Copyright:     2024 by mikel evins
>>>>>>> origin/lispworks-ui
;;;;
;;;; ***********************************************************************

(in-package #:cl-user)

<<<<<<< HEAD
(asdf:defsystem #:nym
  :description "nym: a customizable name generator"
  :author "mikel evins <mikel@evins.net>"
  :license "Apache-2.0"
  :serial t
  :depends-on (:alexandria :folio2)
  :components ((:file "package")
               (:module "util"
                        :serial t
                        :components ((:file "bind")
                                     (:file "sequences")
                                     (:file "strings")
                                     (:file "files")
                                     (:file "samples")))
               (:module "generators"
                        :serial t
                        :components ((:file "chunk-travesty")
                                     (:file "portmanteau")
                                     (:file "phonotactic")))))
=======
(require :asdf)

#+lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf lw:*handle-warn-on-redefinition* :warn))

(asdf:defsystem #:nym
    :description "nym: a customizable name generator"
    :author "mikel evins <mikel@evins.net>"
    :license "Apache-2.0"
    :serial t
    :version (:read-file-form "version.lisp")
    :depends-on (:alexandria :folio3+ :command-line-arguments)
    :components ((:module "src"
                          :serial t
                          :components ((:file "package")
                                       (:file "namer")
                                       #+lispworks (:file "ui"))))
  :build-operation asdf/bundle:program-op
  :build-pathname "nym"
  :entry-point "nym::main")
>>>>>>> origin/lispworks-ui


;;; (asdf:load-system :nym)
