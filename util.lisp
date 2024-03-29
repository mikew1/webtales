;;;; util.lisp
(in-package #:linkdemo)

(defun start-linkdemo (&key
                        (port 8080)
                        (datastore 'linkdemo.pg-datastore:pg-datastore)
                        (datastore-init nil))
  (setf *datastore* (apply #'make-instance datastore datastore-init))  ; [2]
  (init)                                                               ; [3]
  (start '#:linkdemo :port port))                                      ; [4]


;; [2] Will call start-linkdemo like so:

;; With pg-datastore:
;; (linkdemo:start-linkdemo :datastore-init '(:connection-spec ("webtales-linkdemo" "m" "" "localhost")))

;; With redis-datastore:
;; (linkdemo:start-linkdemo :datastore 'linkdemo.redis-datastore:redis-datastore)

;;     The apply here means call make-instance datastore with arg list as needed.
;;     Will look like (make-instance 'pg-datastore :connection-spec '(...))
;; [3] A call to datastore-init, in pg-datastore; in redis-datastore, method exists, but doesn't do anything.
;; [4] :render-method didn't work last time, might need to remove it.
