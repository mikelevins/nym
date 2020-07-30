;;;; ***********************************************************************
;;;;
;;;; Name:          phonotactic.lisp
;;;; Project:       nym
;;;; Purpose:       generating names from phonotactic rules
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:nym-base)

(defparameter $letter-classes-1
  { 'V1 [ 'a 'o 'u ]
        'V2 [ 'e 'i ]
        'C1 [ 'p 't 'k ]
        'C2 [ 'p 'b 't 'd 'k 'g ]
        'C3 [ 's ]
        'C4 [ 'f 'v 's 'z]
        'C5 [ 'w 'l 'r 'y]
        })

(defparameter $phonotactics-1
  { 'R1 ['V1 'C1]
        'R2 ['V2 'C1]
        'R3 ['C2 'V1 'C2]
        'R4 ['C2 'V2 'C2]
        'R5 ['C3 'C1 'V1 'C2]
        'R6 ['C3 'C1 'V1 'C5 'C2]
        'R7 ['C3 'C1 'V2 'C5 'C2]
        'R8 ['C3 'C1 'V1 'C5 'C4]
        'R9 ['C3 'C1 'V2 'C5 'C4]
        'R10 ['C5 'V1 'C5 'C4]
        'R11 ['C5 'V2 'C5 'C4]
        'R12 ['C3 'C2 'C5 'V1 'C5 'C4]
        'R13 ['C3 'C2 'C5 'V2 'C5 'C4]
        'R14 ['C3 'C2 'C5 'V1 'C4]
        'R15 ['C3 'C2 'C5 'V2 'C4]
        'R16 ['C3 'C2 'C5 'V1 'C5]
        'R17 ['C3 'C2 'C5 'V2 'C5]
        'R18 ['C3 'C2 'C5 'V1 'C5 'C2]
        'R19 ['C3 'C2 'C5 'V2 'C5 'C2]
        })

(defun pgen-syllable (letter-classes phonotactics)
  (let* ((rule-keys (fset:convert 'list (fset:domain phonotactics)))
         (rule (fset:@ phonotactics (any rule-keys)))
         (letter-classes (fset:image (lambda (r) (fset:@ letter-classes r))
                                     rule))
         (letters (fset:image (lambda (lc)(symbol-name (any lc)))
                              letter-classes)))
    (reduce #'(lambda (x y)(concatenate 'string x y))
            letters)))

;;; (pgen-syllable $letter-classes-1 $phonotactics-1)

(defun pgen-name (letter-classes phonotactics syllable-count)
  (string-capitalize
   (reduce #'(lambda (x y)(concatenate 'string x y))
           (loop for i from 0 below syllable-count
              collect (pgen-syllable letter-classes phonotactics)))))

;;; (pgen-name $letter-classes-1 $phonotactics-1 (1+ (random 3)))

(defun pgen-names (count)
  (loop for i from 0 below count
     collect (pgen-name $letter-classes-1 $phonotactics-1 (1+ (random 3)))))

;;; (time (pgen-names 200))
