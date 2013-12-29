#lang racket

(require srfi/13
         rackunit
         rackunit/text-ui)

(struct irc-connection (in out))

;; For testing.
(define (fake-irc-connection fake-input)
  (define in (open-input-string fake-input))
  (define out (open-output-string))
  (define conn (irc-connection in out))
  (values in out conn))

;; FIXME: "raw" shouldn't be in here. parse-irc-message should figure it out.
(struct irc-message (prefix command params content raw))

;; Mutable (!) hash for storing message handlers.
(define message-handlers
  (make-hash))

(define (add-message-handler fn)
  (hash-set! message-handlers (gensym) fn))

(define (remove-message-handler id)
  (hash-remove! message-handlers id))

;; TODO: define these with a macro instead of the dumb implementation here.
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

;; This is a mess.
(define (parse-irc-message raw-msg)
  (define parts (string-split raw-msg))
  (define prefix (when (string-prefix? ":" (first parts))
                   (string-drop (first parts) 1)))
  (define cmd (if (> (length parts) 2) (second parts) (first parts)))
  (irc-message prefix cmd #f #f raw-msg))

(define parse-irc-message-test
  (test-suite
   "Parse IRC message."
   (test-case
    "Parse welcome message."
    (define raw-msg ":foo 001 racketbot :what the heck")
    (define msg (parse-irc-message raw-msg))
    (check-equal? (irc-message-command msg) "001")
    (check-equal? (irc-message-prefix msg) "foo"))
   (test-case
    "Parse PING message."
    (define raw-msg "PING foo")
    (define msg (parse-irc-message raw-msg))
    (check-equal? (irc-message-command msg) "PING"))))

;;;;;

(define (print-handler msg conn id)
  (displayln (format "<- ~a" (string-trim-both (irc-message-raw msg)))))

(define (ping-handler msg conn id)
  (define is-ping (equal? (irc-message-command msg) "PING"))
  (when is-ping
    (define server-name (second (split-string (irc-message-raw msg))) ; FIXME
    (define pong (format "PONG ~a" server-name))
    (send-msg pong conn)))

(define (privmsg-handler msg conn id)
  (define is-privmsg (equal? (irc-message-command msg) "PRIVMSG"))
  (when is-privmsg
    (define parts (string-split (irc-message-raw msg))) ; FIXME
    (define channel (third parts)) ; FIXME
    (send-privmsg-msg channel "hi!!!!" conn)))
  
(define (send-msg msg conn)
  (define out (irc-connection-out conn))
  (displayln (format "-> ~a" msg))
  (fprintf out "~a\r\n" msg))

(define (send-init-messages pass conn)
  (when pass
    (send-pass-msg pass conn))
  (send-nick-msg "racketbot" conn)
  (send-user-msg "racketbot" "racketbot" conn))

;; (define (is-welcome? msg)
;;   (define cmd (irc-message-command msg))
;;   (define welcome-code "001")
;;   (when (string-contains-ci cmd welcome-code)
;;     #t))

;; (define (block-on-welcome conn)
;;   (displayln "blocking on welcome")
;;   (define in (irc-connection-in conn))
;;   (define welcome-code "001")           ; FIXME: un-hardcode this?
;;   (let loop ()
;;     (define raw-msg (read-line in))
;;     (displayln (format "<- ~a" raw-msg))
;;     (define msg (or (parse-irc-message raw-msg) #f))
;;     (displayln (format "command is ~a" (irc-message-command msg)))
;;     (unless (or (is-welcome? msg)
;;                 (eof-object? raw-msg))
;;       (loop))))

(define (message-loop conn handlers)
  (define (invoke-handlers line conn)
    (for ([kv (hash->list handlers)])
      ((cdr kv) line conn (car kv))))
  (define (loop)
    (define line (read-line (irc-connection-in conn)))
    (unless (eof-object? line)
      (define msg (parse-irc-message line))
      (invoke-handlers msg conn)
      (loop)))
  (loop))

(define message-loop-tests
  (test-suite
   "Message loop tests."
   (test-case
    "Message loop invokes handlers until eof."
    (define-values (in out conn) (fake-irc-connection "foo\r\nbar\r\nbaz\r\n"))
    (define (handler1 line conn id) (send-msg "quux" conn))
    (define (handler2 line conn id) (send-msg "frotz" conn))
    (define handlers
      (let ([hs (make-hash)])
        (hash-set*! hs (gensym) handler1 (gensym) handler2)
        hs))
    (message-loop conn handlers)
    (define out-str (get-output-string out))
    (check-not-false (string-contains out-str "frotz"))
    (check-equal? 3 (count (curryr string-contains "quux")
                           (string-split out-str))))))
    
(define (irc-connect server #:port [port 6667] #:pass [pass #f])
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define conn
      (let-values ([(in out) (tcp-connect server port)])
        (file-stream-buffer-mode out 'line)
        (irc-connection in out)))
    (send-init-messages pass conn)
    (map add-message-handler
         (list print-handler ping-handler privmsg-handler))
    (send-join-msg "test" conn)
    (thread (thunk (message-loop conn message-handlers)))
    (values conn)))

(module+ main
  (let* ([args (current-command-line-arguments)]
         [server (vector-ref args 0)]
         [pass (vector-ref args 1)])
    (irc-connect server #:pass pass)
    (let loop ()
      (loop))))
