;;;; ***********************************************************************
;;;;
;;;; Name:          namer.lisp
;;;; Project:       nym: an extensible name generator
;;;; Purpose:       name generator
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :nym)

;;; ---------------------------------------------------------------------
;;; namer
;;; ---------------------------------------------------------------------

(defparameter +nametables+ (folio3::hashtable 'equal))

(defun list-namefiles ()
  (let ((project-root (asdf:system-relative-pathname :nymweb "../")))
    (sort (directory (merge-pathnames "data/WOW/*.names" project-root))
        (lambda (x y)(string< (pathname-name x)
                              (pathname-name y))))))

#+test (list-namefiles)

(defun init-nametables ()
  (let* ((namefiles (list-namefiles)))
    (loop for namefile in namefiles
          do (let* ((table (make-nametable namefile)))
               (setf (gethash namefile +nametables+)
                     table)))
    +nametables+))

#+test (init-nametables)

(defmethod nonempty-line? ((s string))
  (let* ((s (string-trim '(#\space #\tab) s)))
    (and (not (char= #\; (elt s 0)))
         (> (length s) 0))))

(defun read-samples (path)
  (folio3::filter #'nonempty-line?
                  (folio3::tap :lines (pathname path))))

(defun chunk-name (namestring)
  (folio3::take-by 3 1 namestring))

(defmethod add-value-for-key ((table hash-table) key value)
  (folio3::set-key! table key
                    ;; adjoin ensures that only one copy of VALUE is ever added
                    (adjoin value (folio3::get-key table key :default nil)
                            :test 'equal)))

(defmethod register-name ((nametable hash-table)(name string))
  (let* ((name-chunks (chunk-name name))
         (end (folio3::last-element name-chunks)))
    ;; the value "" marks the end of a name
    (add-value-for-key nametable end "")
    ;; add each part of the name to the nametable
    (loop for (key val . rest) in (folio3::take-by 2 1 name-chunks)
          do (add-value-for-key nametable key val))))

#+test (chunk-name "Aho")

(defun make-nametable (samples-path)
  (let ((names (read-samples samples-path))
        (table (folio3::hashtable 'equal)))
    (folio3::for-each ((name names))
      (register-name table name))
    table))

#+test (setf $tbl (make-nametable (asdf:system-relative-pathname :nymweb "../data/WOW/Draenei.names")))
#+test (series::collect (folio3::filter #'name-start? (folio3::all-keys $tbl)))

(defmethod name-start? ((chunk string))
  (< 64
     (char-code (folio3::1st chunk))
     91))

(defun extend-name (nametable chunks)
  (let* ((last (folio3::last-element chunks))
         (candidates-for-next (folio3::get-key nametable last)))
    (if candidates-for-next
        (folio3::cat chunks (list (folio3::any candidates-for-next)))
        ;; if no candidates then mark this as the end
        (folio3::cat chunks (list "")))))

(defmethod merge-chunks ((left string)(right string))
  (if (folio3::empty? right)
      left
      (folio3::cat left (folio3::drop 2 right))))

(defun gen-name (nametable)
  (let ((name-chunks (list (folio3::any (series::collect (folio3::filter #'name-start? (folio3::all-keys nametable)))))))
    (loop until (equal "" (folio3::last-element name-chunks))
          do (setf name-chunks (extend-name nametable name-chunks)))
    (reduce #'merge-chunks name-chunks)))

(defun generate-names (nametable count)
  (let ((names nil))
    (loop until (>= (length names) count)
          do (setf names
                   (adjoin (gen-name nametable)
                           names :test #'equal)))
    names))

#+test (defparameter $nametable
         (make-nametable
          (asdf:system-relative-pathname
           :nymweb "../data/WOW/Worgen.names")))
#+test (generate-names $nametable 9)