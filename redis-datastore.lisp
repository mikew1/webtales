;;;; redis-datastore.lisp
(in-package #:linkdemo.redis-datastore)

(defclass redis-datastore ()
  ((host :initarg :host :initform #(127 0 0 1) :accessor host)  ; [1]
   (port :initarg :port :initform 6379 :accessor port)))

(defmethod datastore-init ((datastore redis-datastore)))        ; [2]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; no schema to create - no dao classes like postmodern
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defmethods to add to the generics the restas policy creates
;; including a few helper functions for these
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Schema for our 'schemaless database'
; (viz.: the schema for our database that has no integrity protection)
; --------------------------------------------------------------------
; USER:{id}            - serialised plist, one record per user.
; USER-IDS             - the autoincrement
; USERNAME:{username}  - 'reverse lookup', one record per user.
; POST:{id}            - serialised plist, one record per post.
; POSTS-IDS            - the autoincrement
; UPVOTE:{post-id}     - set of usernames, per post, that have upvoted the post
;                        (replaces a many to many, set makes the pivot unique)

(defun hash-password (password)                                 ; [3]
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

(defun serialize-list (list)                                    ; [4]
  (with-output-to-string (out)
    (print list out)))

(defun deserialize-list (string)
  (let ((read-eval nil))
    (read-from-string string)))

(defun make-key (prefix suffix)                                 ; [5]
  (format nil "~a:~a" (symbol-name prefix) suffix))


(defmethod datastore-find-user ((datastore redis-datastore) username)
  (with-connection (:host (host datastore)
                    :port (port datastore))
    (let ((user-id (red-get (make-key :username username))))      ; [6] ; <- first reverse lookup
      (when user-id
        (deserialize-list (red-get (make-key :user user-id))))))) ; [7] ; <- then user record itself

(defmethod datastore-auth-user ((datastore redis-datastore) username password)  ; [8]
  (let ((user (datastore-find-user datastore username)))
    (when (and user
               (check-password password
                               (getf user :password)
                               (getf user :salt)))
      username)))

(defmethod datastore-register-user ((datastore redis-datastore) username password)
  (with-connection (:host (host datastore)
                    :port (port datastore))
    (unless (datastore-find-user datastore username)
      (let* ((password-salt (hash-password password))
             (id (red-incr :user-ids))
             (record (list :id id                                 ; <- prepare the record to serialize
                           :username username
                           :password (getf password-salt :password-hash)  ; [9]
                           :salt (getf password-salt :salt))))
        (red-set (make-key :user id) (serialize-list record))     ; <- write record to USER:{id}
        (red-set (make-key :username username) id)                ; <- write to the reverse lookup too.
        username))))                                              ; red-set is SET key value
                                                                  ; (not a math set, that's SADD).

(defmethod datastore-upvoted-p ((datastore redis-datastore) link-id user)
  (with-connection (:host (host datastore)
                    :port (port datastore))
    (red-sismember (make-key :upvote link-id) user)))             ; <- 'set is member'. SMEMBER.

(defmethod datastore-upvote ((datastore redis-datastore) link-id user)
  (with-connection (:host (host datastore)
                    :port (port datastore))
    (when (and (datastore-find-user datastore user)
               (not (datastore-upvoted-p datastore link-id user)))
      (when (red-sadd (make-key :upvote link-id) user)           ; <- SADD.  [10]
        link-id))))

(defmethod datastore-post-link ((datastore redis-datastore) url title user)
  (with-connection (:host (host datastore)
                    :port (port datastore))
    (let* ((submitter-id (getf (datastore-find-user datastore user) :id))
           (id (red-incr :posts-ids))
           (link (list :id id                                   ; <- prepare the record to serialize
                       :url url
                       :title title
                       :submitter-id submitter-id)))
      (red-set (make-key :post id) (serialize-list link))       ; <- SET key value, not math set.
      (datastore-upvote datastore (getf link :id) user))))      ; <- upvote it, after posting, as before.


(defun get-all-links/internal ()
  (let ((keys (red-keys (make-key :post "*"))))                 ; <- get all keys matching 'POST:*'
    (loop for key in keys                                       ; <- turn them into a list of lists
          collect (deserialize-list (red-get key)))))           ;    simple enough, perhaps...

(defmethod datastore-upvote-count ((datastore redis-datastore) link-id)
  (with-connection (:host (host datastore)
                    :port (port datastore))
    (red-scard (make-key :upvote link-id))))                    ; <- count elements of a set
                                                                ;    'set cardinality' SCARD.

(defun add-vote-count (datastore links username)                ; [11]
  (loop
    for link in links
    for id = (getf link :id)
    collect (list* :votes (datastore-upvote-count datastore id)
                   :voted-p (datastore-upvoted-p datastore id username)
                   link)))

(defun sort-links (links)                                       ; [12]
  (sort links #'>
        :key #'(lambda (link) (getf link :votes))))

(defmethod datastore-get-all-links ((datastore redis-datastore) &optional username)  ; [13]
  (with-connection (:host (host datastore)
                    :port (port datastore))
    (sort-links
      (add-vote-count datastore
                      (get-all-links/internal)
                      (or username "")))))

;; [1]  host is given as a vector denoting the ip address.
;; [2]  nothing to initialise, as we needed to in postgres, i.e. check tables
;;      exist. So this method is empty. Restas policy needs it to exist though.
;; [3]  These are the same as in pg-datastore; can, as an exercise, "move these
;;      to a separate package and separate file to eliminate the duplication,
;;      if it bugs you".
;; [4]  "fortunately for us, lisp is itself a kind of serialization format"
;;      here we print lisp objects to a string, then read them back as lisp objects.
;; [5]  to generate keys like "USER:1" and "USER:2". symbol-name here seems to just
;;      upper case it. also: it converts :username to USERNAME.
;; [6]  :username is here used to pass this as a keyword, not a variable, like quote.
;;      This first uses the 'reverse lookup', to look up by username & get the user id.
;; [7]  Then another redis get to get the user record, with the id.
;;      Note then, this find-user command is by username, not id.
;;      That conforms to the interface we originally wrote for pg.
;; [8]  Remember these are all generics, specialised on being called with redis-datastore.
;;      That is, dispatching on datastore as redis-datastore.
;;      This is almost the same code as with postmodern.
;; [9]  remember password-salt is a plist, not a single value, salt is the single value.
;; [10] not convinced the check in advance is needed, because its a SET, that's the whole
;;      point of a set, you can only add it once.
;; [11] same as for pg-datastore.
;; [12] same as for pg-datastore.
;; [13] same as before except for redis connection instead of pg.