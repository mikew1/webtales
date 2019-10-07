;;;; util.lisp
(in-package #:linkdemo)

(defun logged-on-p ()
  (hunchentoot:session-value :username)) ; <- always rtns name of user,
                                         ;    so can use it in displaying
                                         ;    username in html-frame (layout file).

(defun log-in (username &optional (redirect-route 'home))   ; [1]
  (hunchentoot:start-session)
  (setf (hunchentoot:session-value :username) username)
  (redirect redirect-route))

(defun log-out (&optional (redirect-route 'home))
  (setf (hunchentoot:session-value :username) nil)
  (redirect redirect-route))

(defun start-linkdemo (&key
                        (port 8080)
                        (datastore 'linkdemo.pg-datastore:pg-datastore)
                        (datastore-init nil))
  (setf *datastore* (apply #'make-instance datastore datastore-init))  ; [2]
  (init)                                                               ; [3]
  (start '#:linkdemo :port port))                                      ; [4]

;; [1] Note that redirect simply takes a quoted route name, neat.
;;     Could hardly be simpler.
;; [2] Will call start-linkdemo like so:

;; (linkdemo:start-linkdemo :datastore-init '(:connection-spec ("webtales-linkdemo" "m" "" "localhost")))

;;     The apply here means call make-instance datastore with arg list as needed.
;;     Will look like (make-instance 'pg-datastore :connection-spec '(...))
;; [3] A call to datastore-init, in pg-datastore.
;; [4] :render-method didn't work last time, might need to remove it.


;;


