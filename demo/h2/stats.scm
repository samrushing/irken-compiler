;; -*- Mode: Irken -*-

;; this should probably be renamed 'events.scm'

;; what kind of info do we want?
;; top level: address pair.
;; 1) unsuccessful connections
;;   tls packets? or just packets?
;; 2) successful connections
;;   (sock.get-info) sexp
;;   request count?  or just stats about requests in general?
;;     [maybe just all the headers]

;; rethinking this a bit.
;; rather than carefully shuffling around optional objects
;;   and modeling the lifecycle of a connection, let's just
;;   use the addr map to record 'events'.  that way we can
;;   handle cases like a successful h2 connection that handles
;;   several requests and then gets EOF.

(datatype conn-event
  (:tls-error (maybe tls-alert-desc) string)
  (:h2-error string)
  (:os-error string)
  (:tls-good sexp)
  (:h2-request (list (:tuple string string)))
  ;; for the port 80 redirector
  (:redirect string)
  )

(define (make-server-history length)
  (let ((fifo (queue/make))
        (map (tree:empty)))

    (define (add addr)
      (let ((entry {addr=addr events=(list:nil)}))
        (queue/push! fifo entry)
        (tree/insert! map string-cmp addr entry)
        (when (> fifo.len length)
          (let ((item (queue/pop! fifo)))
            (match item with
              (maybe:yes item) -> (tree/delete! map string-cmp item.addr)
              (maybe:no)       -> #u)))
        entry))

    (define (get addr)
      (tree/member map string-cmp addr))

    (define (get-all)
      (queue->list fifo))

    (define (clear)
      (set! fifo (queue/make))
      (set! map (tree:empty)))

    {add=add get=get get-all=get-all clear=clear}

    ))

(define (note-request request)
  (let ((info (request.get-info)))
    (match (*stats*.get info.addr) with
      (maybe:yes entry)
      -> (push! entry.events (conn-event:h2-request request.headers))
      (maybe:no)
      -> #u
      )))
