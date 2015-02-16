;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :trivial-graph-canonization-test)

(defsuite* :trivial-graph-canonization-test)

(defun gen-probability ()
  (lambda () (random 1.0)))

(defun gen-graph (&key (class 'digraph)
                    (order-gen (gen-integer :min 2 :max 5))
                    (edge-probability-gen (gen-probability)))
  (lambda ()
    (let* ((order (funcall order-gen))
           (nodes (iota order))
           (edge-probability (funcall edge-probability-gen)))
      (populate (make-instance class)
                :nodes nodes
                :edges
                (remove-if-not
                 (lambda (x) (declare (ignore x))
                   (< (random 1.0) edge-probability))
                 (map-product #'list nodes nodes))))))

(defun shuffle-graph (graph)
  (let ((nodes* (shuffle (copy-list (nodes graph)))))
    (populate (make-instance (class-of graph))
              :nodes nodes*
              :edges (edges graph))))

(deftest simple.1
  (is (equal #*0
             (graph-canonization (populate (make-instance 'graph) :nodes '(a)))))
  (is (equal #*0000
             (graph-canonization (populate (make-instance 'graph) :nodes '(a b)))))
  (is (equal #*000000000
             (graph-canonization (populate (make-instance 'graph) :nodes '(a b c))))))

(deftest simple.2
  (is (equal #*1
             (graph-canonization (populate (make-instance 'graph)
                                           :nodes '(a) :edges '((a a))))))
  (is (equal #*1111
             (graph-canonization (populate (make-instance 'graph)
                                           :nodes '(a b) :edges '((a a) (b b) (a b))))))
  (is (equal #*100010001
             (graph-canonization (populate (make-instance 'graph)
                                           :nodes '(a b c) :edges '((a a) (b b) (c c)))))))

(deftest simple.3
  (is (equal #*0
             (graph-canonization (populate (make-instance 'digraph) :nodes '(a)))))
  (is (equal #*0000
             (graph-canonization (populate (make-instance 'digraph) :nodes '(a b)))))
  (is (equal #*000000000
             (graph-canonization (populate (make-instance 'digraph) :nodes '(a b c))))))

(deftest simple.4
  (is (equal #*1
             (graph-canonization (populate (make-instance 'digraph)
                                           :nodes '(a) :edges '((a a))))))
  (is (equal #*1111
             (graph-canonization (populate (make-instance 'digraph)
                                           :nodes '(a b) :edges '((a a) (b b) (a b) (b a))))))
  (is (equal #*100010001
             (graph-canonization (populate (make-instance 'digraph)
                                           :nodes '(a b c) :edges '((a a) (b b) (c c)))))))

(deftest shuffle-graph.isomorphic
  (for-all ((graph (gen-graph :class 'graph)))
    (is (equal (graph-canonization graph)
               (graph-canonization (shuffle-graph graph))))))

(deftest shuffle-digraph.isomorphic
  (for-all ((graph (gen-graph :class 'digraph)))
    (is (equal (graph-canonization graph)
               (graph-canonization (shuffle-graph graph))))))

(deftest graph.non-isomorphic
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (graph (non-isomorphic-graphs))
      (let ((canonization (graph-canonization graph)))
        (when (gethash canonization seen)
          (fail "nodes ~S edges ~S gives ~S, which is already known"
                (nodes graph) (edges graph) canonization))
        (setf (gethash canonization seen) t)))))

(deftest digraph.non-isomorphic
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (graph (non-isomorphic-digraphs))
      (let ((canonization (graph-canonization graph)))
        (when (gethash canonization seen)
          (fail "nodes ~S edges ~S gives ~S, which is already known"
                (nodes graph) (edges graph) canonization))
        (setf (gethash canonization seen) t)))))
