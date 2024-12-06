;;;; ***********************************************************************
;;;;
;;;; Name:          namebot.lisp
;;;; Project:       nym: an extensible name generator
;;;; Purpose:       the nym name server
;;;; Author:        mikel evins
;;;; Copyright:     2019 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :nym-server)



(hunchentoot:define-easy-handler (options :uri "/") ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (out)
    (format out "Nym")))

(hunchentoot:define-easy-handler (options :uri "/options") ()
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (out)
    (st-json:write-json (nym-base::list-languages (nym-base::nym-data-directory)) out)))

(hunchentoot:define-easy-handler (names :uri "/samples") (language)
  (setf (hunchentoot:content-type*) "application/json")
  (with-output-to-string (out)
    (st-json:write-json (nym-base::read-lines (merge-pathnames (concatenate 'string language ".names")
                                                               (nym-base::nym-data-directory)))
                        out)))

(hunchentoot:define-easy-handler (names :uri "/generate") (language count)
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((language-path (merge-pathnames (concatenate 'string language ".names")
                                         (nym-base::nym-data-directory)))
         (sample-names (nym-base::read-lines language-path))
         (tidied-names (nym-base::tidy-lines sample-names))
         (tmap (nym-base::make-travesty-map tidied-names))
         (name-count (parse-integer count))
         (names (sort (nym-base::generate-names tmap name-count)
                      #'string<)))
    (with-output-to-string (out)
      (st-json:write-json names out))))

;;; (setf hunchentoot::*catch-errors-p* nil)
;;; (setf $server (make-instance 'hunchentoot:easy-acceptor :port 9001))
;;; (hunchentoot:start $server)
;;; (hunchentoot:stop $server)
