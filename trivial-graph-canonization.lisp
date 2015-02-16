;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :trivial-graph-canonization)

(defun build-vector (graph order nodes)
  (let ((matrix (make-array (list order order) :element-type 'bit)))
    (iter
      (for a in nodes)
      (for i upfrom 0)
      (iter
        (for b in nodes)
        (for j upfrom 0)
        (setf (aref matrix i j)
              (if (graph:has-edge-p graph (list a b)) 1 0))))
    (make-array (* order order) :displaced-to matrix :element-type 'bit)))

(defun vector-lex-< (a b)
  (iter
    (for i from 0 below (length a))
    (when (< (aref a i) (aref b i))
      (return t))
    (when (> (aref a i) (aref b i))
      (return nil))))

(defun graph-canonization (graph)
  (let* ((nodes (graph:nodes graph))
         (order (length nodes)))
    (let (list)
      (map-permutations
       (lambda (nodes)
         (push (build-vector graph order nodes) list))
       nodes :copy nil)
      (first (sort list #'vector-lex-<)))))
