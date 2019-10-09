;;;; linkdemo.lisp
(in-package #:linkdemo)

(define-route home ("")
  (html-frame
    (list :title "Linkdemo"
          :body (home-page (get-all-links (logged-on-p))))))     ; [1]

(define-route submit ("submit")
  (html-frame
    (list :title "Submit a link"
          :body (submit-form))))

(define-route submit/post ("submit" :method :post)
  (let ((link (post-link (hunchentoot:post-parameter "url")      ; [2]
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
;; [2] post-link is the dao method here, find it as datastore-post-link.
;; [3] what's noticeable so far is that route defs are so compact, because
;;     of the superbly concise data layer. with actions so compact,
;;     there's no need for separate controllers, & you have much better code.
;;     how this bears up under further strain is of course the qu.
;;     lisp can probably handle that, so long as the community is present.

