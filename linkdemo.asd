(defpackage #:linkdemo-config (:export #:*base-directory*))
(defparameter linkdemo-config:*base-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))

(asdf:defsystem #:linkdemo
  :serial t
  :description "Your description here"
  :author "Your name here"
  :license "Your license here"
  :depends-on (:RESTAS :SEXML :POSTMODERN :IRONCLAD :BABEL :cl-redis :restas-directory-publisher) ; [1]
  :components ((:file "defmodule")
               (:file "pg-datastore")
               (:file "redis-datastore")
               (:file "util")
               (:file "template")
               (:file "linkdemo")))

;; [1] module is pulled in just the same way as a package; it is a package.
;;     as often in lisp, there's nothing extra, no extra nonsense to complicate.

