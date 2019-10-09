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
  (:use #:cl #:restas #:linkdemo.datastore #:authdemo)     ; <- use the datastore above where find-user
  (:export #:start-linkdemo))                              ;    etc. are defined, plus now sep. auth module.

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

(mount-module -authdemo- (#:authdemo)                           ; [8]
  ;(:render-method 'html-frame)                                 ; [9]
  ;(authdemo::*html-frame* #'html-frame) ; <- had idea to pass html-frame as context here,
  ;                                           but am failing to understand how to refer to it.
  (authdemo::*authenticate-user-function* #'auth-user)  ; <- had to ref internal symbols here '::', book
  (authdemo::*register-user-function* #'register-user)  ;    used just ':', maybe restas code changed.
  (authdemo::*redirect-route* 'home))                           ; [10]

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
;; [8] Mount our new module.
;;     N.B. Upon mounting, restas will automatically generate symbols for each route
;;     within a mounted package within the current package. In our case, these will be:
;;     -authdemo-.login, .login/post, .register, .register/post & .logout
;;     These symbols can now be used in the current package with genurl or redirect.
;;     Exactly as we would want. Great. Now modules can interact without knowing much
;;     about each other. (In laravel, route() was simply in a global namespace, i.e., no
;;     specification of dependencies exists. Kind of ok, maybe, but will ultimately restrict you).
;; [9] In earlier chapters, we removed :render-method in existing code as it wasn't working.
;;     At a guess that may be due to changes in restas code since book was published.
;;     Ideas for fix, simply to get this code to working state:
;;     - If html-frame is not in scope within the module, could just copy that fn across.
;;     - Or, perhaps better, if this makes sense, add it as another context variable,
;;       to just pass it across, nice.
;; [10] A mounted module can use the routes of the module it is mounted within, which means
;;      we can just pass 'home here (not sure how this is implemented - find out if need).