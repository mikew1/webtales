;;;; defmodule.lisp
(restas:define-policy datastore
  (:interface-package #:linkdemo.policy.datastore)
  (:interface-method-template "DATASTORE-~A")
  (:internal-package #:linkdemo.datastore)
  (:internal-function-template "~A")
  (define-method init ()                                    ; [1]
    "Initiate the datastore")
  (define-method find-user (username)                       ; [3]
    "Find the user by username")
  (define-method auth-user (username password)
    "Check if a user exists and has the supplied password")
  (define-method register-user (username password)
    "Register a new user")
  (define-method upvoted-p (link-id username)
    "Check if a user has upvoted a link")
  (define-method upvote (link-id user)
    "Upvote a link")
  (define-method post-link (url title user)
    "Post a new link")
  (define-method get-all-links (&optional user)
    "Get all of the links in the datastore")
  (define-method upvote-count (link-id)
    "Get the number of upvotes for a given link"))

; Bug: (ql:quickload "linkdemo") & got 'Error: The value NIL is not of the expected type STRING.'
; Solution was: macro required (:internal-function-template "~A"), can't omit it as book showed.

(restas:define-module #:linkdemo                            ; [4]
  (:use #:cl #:restas #:linkdemo.datastore)                ; <- use the datastore above where find-user
  (:export #:start-linkdemo))                              ;    etc. are defined.

(defpackage #:linkdemo.pg-datastore                         ; [5]
  (:use #:cl #:postmodern #:linkdemo.policy.datastore)      ; <- use the generic fns that'll be defined
  (:export #:pg-datastore))

(defpackage #:linkdemo.redis-datastore
  (:use #:cl #:redis #:linkdemo.policy.datastore)          ; <- names of methods we must implement reside
  (:export #:redis-datastore))                             ;    here.
                                                           ; [6]
(in-package #:linkdemo)

(defparameter *template-directory*
  (merge-pathnames #P"templates/" linkdemo-config:*base-directory*))

(defparameter *static-directory*
  (merge-pathnames #P"static/" linkdemo-config:*base-directory*))

(sexml:with-compiletime-active-layers
  (sexml:standard-sexml sexml:xml-doctype)
  (sexml:support-dtd
    (merge-pathnames "html5.dtd" (asdf:system-source-directory "sexml"))
    :<))

(mount-module -static- (#:restas.directory-publisher)
  (:url "static")
  (restas.directory-publisher:*directory* *static-directory*))  ; [7]

;; [1] N.B. This is not defmethod, the lisp form, define-method is part of the
;;     define-policy syntax. What we see here is a giant 'policy', which sets
;;     up, so it seems, an inversion of control.
;;     Let's see it in action first before trying to understand it more right now.
;;     It looks like with care you could get some pretty fine abstractions in lisp
;;     if you know what it is you want.
;; [2] This is your data access layer. No 'models' in sight. Let's see what it can do.
;; [3] These will be defined in terms of generic functions. Those generics will have
;;     a method for each datastore class, e.g. postgres or redis. Decoupling job done.
;;     *datastore* will be a global var set in our case to postgres, which all these
;;     generics will dispatch on. That's done behind the scenes by restas, after you
;;     define a policy as above, when you call a method defined with define-method.
;; [4] Then we need to define the restas module for our application.
;; [5] "linkdemo uses the internal package linkdemo.datastore, where all the data
;;     access fns like find-user are defined. linkdemo.pg-datastore uses the
;;     interface package where the generic functions we need to implement the
;;     methods are defined". (p44).
;; [6] the symbol redis-datastore names the class we use to access the datastore.
;; [7] syntax here is a (variable binding). *directory* is the directory to serve,
;;     *static-directory* is the directory to bind it to.
;;     note it looks like a package can have its own globals, referred to like
;;     restas.directory-publisher:*directory*.
;;     if that's correct, then the list given to mount-module is simply a varable binding.
;;     (variable to-bind-to)