;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :trivial-graph-canonization)

(defun build-matrix (graph order nodes)
  (let ((matrix (make-array (list order order) :element-type 'bit)))
    (iter
      (for a in nodes)
      (for i upfrom 0)
      (iter
        (for b in nodes)
        (for j upfrom 0)
        (locally
            ;; Prevent CCL bug observed on
            ;; CCL "Version 1.10-r16196  (LinuxX8664)"
            ;; Fault during read of memory address #xB
            ;;    [Condition of type CCL::INVALID-MEMORY-ACCESS]
            ;; by declaring the setf (aref matrix i j)
            ;; with (optimize debug).
            #+(and ccl X86-64) (declare (optimize debug))
            (setf (aref matrix i j)
                  (if (graph:has-edge-p graph (list a b)) 1 0)))))
    matrix))

(defun build-vector (matrix order nodes)
  (declare (type (simple-array bit) matrix))
  (declare (type positive-fixnum order))
  (declare (type list nodes))
  (declare (optimize speed (safety 0)))
  (let ((vector (make-array (the fixnum (* order order))
                            :element-type 'bit)))
    (iter
      (for a in nodes)
      (for i upfrom 0)
      (for i-order = (* i order))
      (declare (fixnum i i-order))
      (iter
        (for b in nodes)
        (for j upfrom 0)
        (declare (fixnum j))
        (setf (aref vector (+ i-order j))
              (aref matrix a b))))
    vector))

(declaim (inline vector-lex-<))
(defun vector-lex-< (a b)
  (declare (type simple-bit-vector a b))
  (declare (optimize speed (safety 0)))
  (iter
    (for i from 0 below (length a))
    (for a-i = (aref a i))
    (for b-i = (aref b i))
    (declare (fixnum i))
    (when (< a-i b-i)
      (return t))
    (when (> a-i b-i)
      (return nil))))

(defun find-minimum (list)
  (declare (optimize speed (safety 0)))
  (iter
    (with result = (first list))
    (for x in (rest list))
    (when (vector-lex-< x result)
      (setq result x))
    (finally (return result))))

(defun graph-canonization-not-zero (graph order nodes)
  (let (list
        (matrix (build-matrix graph order nodes)))
    (map-permutations
     (lambda (nodes)
       (push (build-vector matrix order nodes) list))
     (iota order) :copy nil)
    (find-minimum list)))

(defun graph-canonization (graph)
  (let* ((nodes (graph:nodes graph))
         (order (length nodes)))
    (if (zerop order)
        (make-array 0 :element-type 'bit)
        (graph-canonization-not-zero graph order nodes))))
