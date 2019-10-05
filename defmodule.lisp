;;;; defmodule.lisp

(restas:define-module #:linkdemo
  (:use #:cl))

(in-package #:linkdemo)

(defparameter *template-directory*
  (merge-pathnames #P"templates/" linkdemo-config:*base-directory*))

(defparameter *static-directory*
  (merge-pathnames #P"static/" linkdemo-config:*base-directory*))


