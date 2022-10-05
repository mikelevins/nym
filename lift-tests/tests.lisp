
;;;; ***********************************************************************
;;;;
;;;; Name:          tests.lisp
;;;; Project:       nym
;;;; Purpose:       sequence tests
;;;; Author:        mikel evins
;;;; Copyright:     2022 by mikel evins
;;;;
;;;; ***********************************************************************


(in-package :cl-user)

(defpackage :lift-tests
  (:use :cl :nym :lift))

(lift:deftestsuite lift-tests () ())

(lift:addtest (lift-tests)
  test-read-samples
  (let* ((samples (nym::read-samples (asdf:system-relative-pathname :nym "data/us.names"))))
    (lift:ensure (listp samples))
    (lift:ensure (not (null samples)))))

(lift:addtest (lift-tests)
  test-gen-100
  (let* ((samples (nym::read-samples (asdf:system-relative-pathname :nym "data/us.names")))
         (names (nym::gen-chunk-travesties samples 100)))
    (lift:ensure (= 100 (length names)))))
