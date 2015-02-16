;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package :asdf-user)

(defsystem :trivial-graph-canonization
  :name "trivial-graph-canonization"
  :description "Trivial graph canonization in O(|V|!) for small graphs."
  :author "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :version #.(with-open-file
                 (vers (merge-pathnames "version.lisp-expr" *load-truename*))
               (read vers))
  :components ((:static-file "version" :pathname #p"version.lisp-expr")
               (:file "package")
               (:file "trivial-graph-canonization" :depends-on ("package"))
               )
  :depends-on (:alexandria :graph :iterate))

(defmethod perform ((op test-op)
                    (system (eql (find-system :trivial-graph-canonization))))
  (oos 'load-op :trivial-graph-canonization-test)
  (funcall (intern "RUN!" "MYAM") :trivial-graph-canonization-test))
