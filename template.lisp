;;;; template.lisp
(in-package #:linkdemo)
(<:augment-with-doctype "html" "" :auto-emit-p t)

(defun html-frame (context)                                 ; [1]
  (<:html
    (<:head (<:title (getf context :title))
            (<:link :rel "stylesheet" :type "text/css" :href "/static/css/style.css"))
    (<:body
      (<:div
        (<:h1 (getf context :title))
        (<:a :href (genurl 'home) "Home") " | "
        (if (logged-on-p)
            (list
              (<:a :href (genurl 'submit) "Submit a link")
              " | "
              (<:a :href (genurl '-authdemo-.logout)        ; [2] <- ref route in -authdemo-
                   (format nil "Logout ~A"
                           (logged-on-p))))
            (list (<:a :href (genurl '-authdemo-.login) "Log in")
                  " or "
                  (<:a :href (genurl '-authdemo-.register) "Register")))
        (<:hr))
      (getf context :body))))

(defun home-page (links)                                    ; [3]
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
             (getf link :votes)
             " "
             (<:a :href (getf link :url) (getf link :title)))))

(defun submit-form ()
  (<:form :action (genurl 'submit/post) :method "post"
          "Title:" (<:br)
          (<:input :type "text" :name "title") (<:br)
          "URL:" (<:br)
          (<:input :type "text" :name "url") (<:br)
          (<:input :type "submit" :value "Submit")))

;; [1] html-frame similar to in blogdemo, but now has register option.
;; [2] Reference to one of the routes which are created as new symbols in this package
;;     when the module authdemo is mounted, with mount-module, in this app/module.
;; [3] home-page takes a list of links, which are plists. It then displays
;;     all the links on separate lines. On each line, is either an upvote link,
;;     or a * if the user isn't logged in, followed by a vote count & the actual
;;     link. If user has already upvoted, instead of upvote, we show a *.