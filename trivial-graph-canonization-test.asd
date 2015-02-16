;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :asdf-user)

(defsystem :trivial-graph-canonization-test
  :name "trivial-graph-canonization-test"
  :description "Tests for trivial-graph-canonization"
  :components ((:module "test"
                :components ((:file "package")
                             (:file "data" :depends-on ("package"))
                             (:file "test" :depends-on ("package")))))
  :depends-on (:trivial-graph-canonization :myam :alexandria :iterate))

(defmethod perform ((op test-op)
                    (system (eql (find-system :trivial-graph-canonization-test))))
  (perform op (find-system :trivial-graph-canonization)))
