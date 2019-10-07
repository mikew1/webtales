;;;; pg-datastore.lisp
(in-package #:linkdemo.pg-datastore)

;; to run in repl: (ql:quickload :postmodern) (use-package :postmodern)
(defclass pg-datastore ()
  ((connection-spec :initarg :connection-spec :accessor connection-spec)))

; Can test in repl with *db*, real app will use *datastore*
; (defparameter *db*
;   (make-instance 'linkdemo.pg-datastore::pg-datastore
;                  :connection-spec '("webtales-linkdemo" "m" "" "localhost")))
; (linkdemo.pg-datastore::datastore-get-all-links *db*)
; -> ((:VOTES 1 :VOTED-P NIL :ID 1 :URL "A link from user1" :SUBMITTER-ID 4))
; (connection-spec *db*)        ; <- returns creds

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; postmodern DAO classes & table defs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass users ()    ; <- DAO classes are part of postmodern            ; [1]
  ((id :col-type serial :reader user-id)
   (name :col-type string :reader user-name :initarg :name)
   (password :col-type string :reader user-password :initarg :password)
   (salt :col-type string :reader user-salt :initarg :salt))
  (:metaclass dao-class)                                                 ; [2]
  (:keys id))                                                            ; [3]
; (dao-table-definition 'users)  ; <- see sql table def.

(defclass links ()
  ((id :col-type serial :reader link-id)
   (url :col-type string :reader link-url :initarg :url)
   (title :col-type string :reader link-title :initarg :title)
   (submitter-id :col-type integer :reader link-submitter-id :initarg :submitter-id))
  (:metaclass dao-class)
  (:keys id))

(deftable links                            ; <- dao-class doesn't let add fks. this is how we do that.
  (!dao-def)                               ; <- inherit field defs from dao class
  (!foreign 'users 'submitter-id 'id))     ; <- add fk

(defclass votes ()
  ((link-id :col-type integer :reader vote-link-id :initarg :link-id)
   (submitter-id :col-type integer :reader vote-submitter-id :initarg :submitter-id))
  (:metaclass dao-class)
  (:keys link-id submitter-id))            ; <- composite key, no surrogate id here

(deftable votes
  (!dao-def)
  (!foreign 'links 'link-id 'id)
  (!foreign 'users 'submitter-id 'id))

(defmethod datastore-init ((datastore pg-datastore))                     ; [4]
  (with-connection (connection-spec datastore)
    (unless (table-exists-p 'users)
      (execute (dao-table-definition 'users)))    ; <- uses the dao def
    (unless (table-exists-p 'links)
      (create-table 'links))                      ; <- with fks, uses create-table instead
    (unless (table-exists-p 'votes)
      (create-table 'votes))))
; (datastore-init *db*)          ; <- initialize the db

; insert using dao:
; (with-connection (connection-spec *db*)
;   (insert-dao (make-instance 'users :name "user" :password "pass" :salt "1232")))
; ; query, dao way
; (with-connection (connection-spec *db*)
;   (select-dao 'users))
; ; query, sql way with s-sql
; (with-connection (connection-spec *db*)
;   (query (:select :* :from 'users)))
; ; query, sql way with s-sql, plists
; (with-connection (connection-spec *db*)
;     (query (:select :* :from 'users) :plists))
; ; query, sql way with s-sql, alists
; (with-connection (connection-spec *db*)
;     (query (:select :* :from 'users) :alists))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defmethods to add to the generics the restas policy creates
;; including a few helper functions for these
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hash-password (password)
  "returns a plist with a password hash & a salt, ready to be stored in db."
  (multiple-value-bind (hash salt)
    (ironclad:pbkdf2-hash-password (babel:string-to-octets password))
    (list :password-hash (ironclad:byte-array-to-hex-string hash)
          :salt (ironclad:byte-array-to-hex-string salt))))

(defun check-password (password password-hash salt)
  "take password, hash & salt, & see if hashed password, using salt, matches."
  (let ((hash (ironclad:pbkdf2-hash-password
                (babel:string-to-octets password)
                :salt (ironclad:hex-string-to-byte-array salt))))
    (string= (ironclad:byte-array-to-hex-string hash)
             password-hash)))

; restas:define-policy will have defined the actual generics. what we're
; left to do is define the methods for our pg-datastore

; user handling logic
(defmethod datastore-find-user ((datastore pg-datastore) username)
  "find user in pg datastore. note :plist returns one record, unlike :plists"
  (with-connection (connection-spec datastore)
    (query (:select :* :from 'users
                    :where (:= 'name username))
           :plist)))

(defmethod datastore-auth-user ((datastore pg-datastore) username password)    ; [5]
  "authenticate user by checking for password match. return username or nil."
  (let ((user (datastore-find-user datastore username)))
    (when (and user
               (check-password password (getf user :password)
                                        (getf user :salt)))
      username)))

(defmethod datastore-register-user ((datastore pg-datastore) username password)
  "register the user if not already registered, returning username if success, or nil."
  (with-connection (connection-spec datastore)
    (unless (datastore-find-user datastore username)
      (let ((password-salt (hash-password password)))
        (when
          (save-dao
            (make-instance 'users
                           :name username
                           :password (getf password-salt :password-hash)
                           :salt (getf password-salt :salt)))
          username)))))

; handling links
(defmethod datastore-upvoted-p ((datastore pg-datastore) link-id user)
  "has the link been upvoted by the user, rtn link id & username"
  (with-connection (connection-spec datastore)
    (query (:select :link-id :users.name :from 'votes 'users   ; <- 'implicit join'
                    :where (:and (:= 'users.id 'submitter-id)
                                 (:= 'users.name user)
                                 (:= 'link-id link-id)))
           :plist)))

(defmethod datastore-upvote ((datastore pg-datastore) link-id user)
  "upvote the link by the user so long as not already upvoted, rtn link id or nil."
  (with-connection (connection-spec datastore)
    (let ((submitter-id (getf (datastore-find-user datastore user) :id)))
      (when (and submitter-id
                 (not (datastore-upvoted-p datastore link-id user)))
          (when (save-dao (make-instance 'votes
                                         :link-id link-id
                                         :submitter-id submitter-id))
            link-id)))))

(defmethod datastore-post-link ((datastore pg-datastore) url title user)
  "post and upvote by submitting user. need to use dao, as query won't rtn inserted value."
  (with-connection (connection-spec datastore)
    (let* ((submitter-id (getf (datastore-find-user datastore user) :id))
           (link (make-instance 'links
                                :url url
                                :title title
                                :submitter-id submitter-id)))
      (save-dao link)
      (datastore-upvote datastore (link-id link) user))))

(defun get-all-links/internal ()
  "getting all links has three steps: select, check upvote count, then sort. this is first step."
  (query (:select :* :from 'links) :plists))

(defmethod datastore-upvote-count ((datastore pg-datastore) link-id)
  "the upvote count for the link"
  (with-connection (connection-spec datastore)
    (query (:select (:count link-id) :from 'votes
                    :where (:= link-id 'link-id))
           :single)))

(defun add-vote-count (datastore links username)
  "augment link plist with vote count & voted-p, whether logged user has voted on link"
  (loop
    for link in links
    for id = (getf link :id)
    collect (list* :votes (datastore-upvote-count datastore id)
                   :voted-p (datastore-upvoted-p datastore id username)
                   link)))

(defun sort-links (links)
  (sort links #'>
        :key #'(lambda (link) (getf link :votes))))

(defmethod datastore-get-all-links ((datastore pg-datastore) &optional username)
  "and finally we can define our method"
  (with-connection (connection-spec datastore)
    (sort-links
      (add-vote-count datastore
                      (get-all-links/internal)
                      (or username "")))))  ; <- &optional & or to stop upvoted-p
                                            ;    choking on nil for unlogged.

;; [1] The difference between a standard class definition and a dao class
;;     is that we have a :col-type option to our slots. That specifies what db
;;     type we awnt to create. DAO classes are part of postmodern.
;; [2] Cool part: metaobject protocol means even classes are instances of classes.
;;     Postmodern provides a dao-class which tells these CLOS classes how to
;;     save themselves into a database.
;;     It's that dao-class metaclass that lets us add :col-type to our slots.
;;     Superbly cool..
;; [3] id will be a primary key.
;; [4] book had stray quote here, error (i think)
;;     defmethod takes a type which specialises it. this is not a default value.
;; [5] N.B. Writing an auth system should not be a task for every developer.
;;     This should be part of a f/w. It might be slightly different in lisp,
;;     not sure yet.