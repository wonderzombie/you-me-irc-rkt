;; #lang racket

(require srfi/13)

(struct irc-connection (in out in-chan))
(struct irc-message (prefix command params content))

;; Mutable hash for storing message handlers.
(define message-handlers
  (make-hash))

(define (add-message-handler fn)
  (hash-set! message-handlers (gensym) fn))

(define (remove-message-handler id)
  (hash-remove! message-handlers id))

(define irc-numerics
  (hash "001" 'RPL_WELCOME 
        "004" 'RPL_MYINFO))

(define (send-pass-msg pass out)
  (define msg (format "PASS ~a" pass))
  (send-msg msg out))

(define (send-nick-msg nick out)
  (define msg (format "NICK ~a" nick))
  (send-msg msg out))

(define (send-user-msg user realname out)
  (define msg (format "USER ~a 0 * :~a" user realname))
  (send-msg msg out))

(define (send-join-msg channel out)
  (define msg (format "JOIN #~a" channel))
  (send-msg msg out))

(define (send-privmsg-msg target content out)
  (define msg (format "PRIVMSG ~a :~a" target content))
  (send-msg msg out))

;;;;;

;; Incomplete.

(define (parse-irc-message msg)
  (define parts (string-split msg))
  (irc-message (first parts) (second parts) #f #f))

;;;;;

;; Two-tiered. You have a message handler which takes a msg and a conn.
;; The handler does something w/ message and then invokes a generic send w/ conn.
  
(define (send-msg msg conn)
  (define out (irc-connection-out conn))
  (displayln (format "-> ~a" msg))
  (fprintf out "~a\r\n" msg))

(define (handle-msg msg conn)
  (displayln msg))

(define (close-both cin cout)
  (close-input-port cin)
  (close-output-port cout))

;; For REPL.
(define (dumb-connect)
  (define-values (in out) (tcp-connect "" 6667)) ; FIXME
  (file-stream-buffer-mode out 'line)
  (define conn (irc-connection in out #f))
  (send-init-messages conn)
  (block-on-welcome conn)
  (send-join-msg "test" conn)
  (values in out))

(define (send-init-messages conn)
  (send-pass-msg "" conn)
  (send-nick-msg "racketbot" conn)
  (send-user-msg "racketbot" "racketbot" conn))

(define (block-on-welcome conn)
  (define in (irc-connection-in conn))
  (define welcome-code "001")           ; FIXME: un-hardcode this?
  (let loop ()
    (define raw-msg (read-line in))
    (displayln (format "<- ~a" raw-msg))
    (define msg (parse-irc-message raw-msg))
    (define cmd (irc-message-command msg))
    (when (or (not (string-contains-ci cmd welcome-code))
      (loop))))

(define (message-loop conn handlers)
  (define in (irc-connection-in conn))
  (define out (irc-connection-out conn))
  (define (invoke-handlers line)
    (for ([kv (hash->list handlers)])
      ((cdr kv) line)))
  (define (loop)
    (define line (read-line in))
     (cond
      [(eof-object? line) (displayln "eof, done")]
      [else (invoke-handlers line conn)]))
  (loop))
  
(define (irc-connect server #:port [port 6667])
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-connect server port))
    (file-stream-buffer-mode out 'line)
    (define conn (irc-connection in out #f))
    (define (closer) (close-both in out))
    (send-init-messages conn)
    (block-on-welcome conn)
    (send-join-msg "test" conn)
    (thread (thunk (message-loop conn message-handlers)))
    (values conn closer)))

;; (module+ main
;;   (irc-connect ""))                     ; FIXME
