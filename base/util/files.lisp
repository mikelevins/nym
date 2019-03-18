;;;; ***********************************************************************
;;;;
;;;; Name:          files.lisp
;;;; Project:       nym
;;;; Purpose:       file helpers
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym-base)

(defun nym-data-directory ()
  (asdf/system:system-relative-pathname :nym-base "data/"))

(defun nym-data-file (filename)
  (merge-pathnames filename (nym-data-directory)))

;;; (nym-data-directory)


(defmethod list-languages ((dir pathname))
  (let* ((language-files (directory (merge-pathnames "*.names" dir)))
         (languages (loop for file in language-files
                          collect (pathname-name file))))
    (sort languages #'string<)))

(defmethod read-lines ((path pathname))
  (with-open-file (in path :direction :input)
    (loop for
          line = (read-line in nil nil nil)
          then (read-line in nil nil nil)
          while line
          collect line)))

(defmethod read-lines ((path string))
  (read-lines (pathname path)))

(defun tidy-lines (lines)
  (sort (delete-duplicates
         (delete nil (filter (complement #'empty-line?) lines))
         :test #'string=)
        #'string<))
