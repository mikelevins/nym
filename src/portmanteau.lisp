;;;; ***********************************************************************
;;;;
;;;; Name:          portmanteau.lisp
;;;; Project:       nym
;;;; Purpose:       simple portmanteau generator
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym)

(defun empty-text? (str)
  (let ((trimmed (string-trim '(#\space) str)))
    (or (string= trimmed "")
        (char= #\# (elt trimmed 0)))))

(defmethod read-samples ((filename pathname))
  (let* ((lines (with-open-file (in filename)
                  (loop for line = (read-line in nil nil nil) then (read-line in nil nil nil)
                     while line
                     collect line)))
         ;; filter out empty and comment lines
         (filtered (remove-if #'empty-text?
                              lines)))
    ;; sort the output
    (sort filtered #'string<)))

(defmethod read-samples ((filename string))
  (read-samples (pathname filename)))

(defun any (seq)
  (elt seq (random (length seq))))

(defmethod any-triple ((string string))
  (if (<= (length string) 3)
      string
    (let* ((start (random (- (length string) 2)))
           (end (+ start 3)))
      (subseq string start end))))

(defun match-sample (sample samples)
  (any (remove-if-not (lambda (s)(search sample s))
                      samples)))

(defun split-word (word chunk)
  (let ((pos (search chunk word)))
    (if pos
        (values (subseq word 0 pos)
                chunk
                (subseq word (+ pos (length chunk))))
        (error "Subsequence ~S not found in word ~S" chunk word))))

(defun gen-portmanteau (samples)
  (let* ((start-word (any samples))
         (seed (any-triple start-word))
         (end-word (match-sample seed samples)))
    (multiple-value-bind (left seed discard-right)(split-word start-word seed)
      (multiple-value-bind (discard-left seed right)(split-word end-word seed)
        (concatenate 'string left seed right)))))

(defun gen-portmanteaus (samples count)
  (loop for i from 0 below count collect (gen-portmanteau samples)))