;;;; linkdemo.lisp
(in-package #:linkdemo)

(define-route home ("")
  (list :title "Linkdemo"
        :body (home-page (get-all-links (logged-on-p)))))  ; [1]

(define-route login ("login")                              ; [2]
  (list :title "Log in"
        :body (login-form)))

(define-route login/post ("login" :method :post)
  (let ((user (auth-user (hunchentoot:post-parameter "username")
                         (hunchentoot:post-parameter "password"))))
    (if user
        (log-in user)
        (redirect 'login))))


(define-route register ("register")
  (list :title "register"
        :body (register-form)))

(define-route register/post ("register" :method :post)
  (let ((user (register-user (hunchentoot:post-parameter "username")
                             (hunchentoot:post-parameter "password"))))
    (if user
        (log-in user)
        (redirect 'register))))

(define-route logout ("logout")
  (log-out))


(define-route submit ("submit")
  (list :title "Submit a link"
        :body (submit-form)))

(define-route submit/post ("submit" :method :post)
  (let ((link (post-link (hunchentoot:post-parameter "url")      ; [3]
                         (hunchentoot:post-parameter "title")
                         (logged-on-p))))
    (if link
        (redirect 'home)
        (redirect 'submit))))

(define-route upvote-link ("upvote/:id")
  (:sift-variables (id #'parse-integer))
  (when (logged-on-p)
    (upvote id (logged-on-p)))
  (redirect 'home))


;; [1] home-page returns the html, get-all-links is call to new database layer.
;;     logged-on-p returns logged username or nil.
;;     note that get-all-links actually calls datastore-get-all-links,
;;     via the clever policy. the goal seems to be hiding abstractions,
;;     & making what you write as a client as simple as possible. see p57 top.
;; [2] Simply return the login-form.
;; [3] post-link is the dao method here, find it as datastore-post-link.
;; [4] what's noticeable so far is that route defs are so compact, because
;;     of the superbly concise data layer. with actions so compact,
;;     there's no need for separate controllers, & you have much better code.
;;     how this bears up under further strain is of course the qu.
;;     lisp can probably handle that, so long as the community is present.

