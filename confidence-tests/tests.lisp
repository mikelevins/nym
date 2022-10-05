
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

(defpackage :confidence-tests
  (:use :cl :nym :org.melusina.confidence))

(in-package :confidence-tests)

(define-testcase test-read-samples ()
  (let* ((samples (nym::read-samples (asdf:system-relative-pathname :nym "data/us.names"))))
    (assert-t* (listp samples))
    (assert-t* (not (null samples)))))

(define-testcase test-gen-100 ()
  (let* ((samples (nym::read-samples (asdf:system-relative-pathname :nym "data/us.names")))
         (names (nym::gen-chunk-travesties samples 100)))
    (assert-t* (= 100 (length names)))))

(define-testcase run-tests ()
  (test-read-samples)
  (test-gen-100))

#+nil (run-tests)
