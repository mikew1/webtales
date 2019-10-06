;;;; template.lisp
(in-package #:linkdemo)
(<:augment-with-doctype "html" "" :auto-emit-p t)

(defun html-frame (context)                                 ; [1]
  (<:html
    (<:head (<:title (getf context :title)))
    (<:body
      (<:div
        (<:h1 (getf context :title))
        (<:a :href (genurl 'home) "Home") " | "
        (if (logged-on-p)
            (list
              (<:a :href (genurl 'submit) "Submit a link")
              " | "
              (<:a :href (genurl 'logout)
                   (format nil "Logout ~A"
                           (logged-on-p))))
            (list (<:a :href (genurl 'login) "Log in")
                  " or "
                  (<:a :href (genurl 'register) "Register")))
        (<:hr))
      (getf context :body))))

(defun home-page (links)                                    ; [2]
  (loop
    for link in links
    collect
      (<:div (if (logged-on-p)
                 (if (getf link :voted-p)
                     "*"
                     (<:a :href (genurl 'upvote-link :id (getf link :id))
                          "upvote"))
                 "*")
             " "
             (get link :votes)
             " "
             (<:a :href (getf link :url) (getf link :title)))))

(defun login-form ()                                        ; [3]
  (<:form :action (genurl 'login/post) :method "post"
      "User name:" (<:br)
      (<:input :type "text" :name "username") (<:br)
      "Password:" (<:br)
      (<:input :type "password" :name "password") (<:br)
      (<:input :type "submit" :value "Log in")))

(defun register-form ()
  (<:form :action (genurl 'register/post) :method "post"
      "User name:" (<:br)
      (<:input :type "text" :name "username") (<:br)
      "Password:" (<:br)
      (<:input :type "password" :name "password") (<:br)
      (<:input :type "submit" :value "Register")))

(defun submit-form ()
  (<:form :action (genurl 'submit/post) :method "post"
          "Title:" (<:br)
          (<:input :type "text" :name "title") (<:br)
          "URL:" (<:br)
          (<:input :type "text" :name "url") (<:br)
          (<:input :type "submit" :value "Submit")))

;; [1] html-frame similar to in blogdemo, but now has register option.
;; [2] home-page takes a list of links, which are plists. It then displays
;;     all the links on separate lines. On each line, is either an upvote link,
;;     or a * if the user isn't logged in, followed by a vote count & the actual
;;     link. If user has already upvoted, instead of upvote, we show a *.
;; [3] Lovely and compact. Half the LOC of a template lang, & don't need sep. files.