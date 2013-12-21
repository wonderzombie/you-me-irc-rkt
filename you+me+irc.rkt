;; #lang racket

(require srfi/13)

(define (make-pass-msg pass)
  (format "PASS ~a" pass)) 

(define (make-nick-msg nick)
  (format "NICK ~a" nick))

(define (make-user-msg user realname)
  (format "USER ~a 0 * :~a" user realname))

(define (make-join-msg chan)
  (format "JOIN #~a" chan))

(define (make-privmsg-msg msgtarget msg)
  (format "PRIVMSG ~a :~a" msgtarget msg))

(struct irc-message (prefix command params content))

;; Incomplete
(define (parse-irc-message msg)
  (define parts (string-split msg))
  (irc-message (first parts) #f #f #f))
  
(define init-messages
  (list (make-pass-msg "")              ; FIXME
        (make-nick-msg "racketbot")
        (make-user-msg "racketbot" "racket bot")
        (make-join-msg "test")))

(define (send-msg msg out)
  (displayln msg)
  (fprintf out "~a\r\n" msg))

(define (handle-msg msg)
  (displayln msg))

;; Convenience for REPL.
(define (close-both cin cout)
  (close-input-port cin)
  (close-output-port cout))

;; For REPL.
(define (dumb-connect)
  (define-values (cin cout) (tcp-connect "" 6667)) ; FIXME
  (file-stream-buffer-mode cout 'line)
  (map (lambda (m) (send-msg m cout)) init-messages)
  (displayln (read-line cin))
  (values cin cout))

(define (send-init-messages out)
  (define (send msg) (send-msg m out))
  (map send init-messages))

(define (irc-connect server #:port [port 6667])
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (cin cout) (tcp-connect server port))
    (file-stream-buffer-mode cout 'line)
    (send-init-messages cout)
    (send-msg (make-join-msg "test") cout)
    (define (loop)
      (let ([msg (read-line cin)])
        (when (not (equal? eof msg))
          (displayln msg)
          (loop))))
    (loop)))

;; (module+ main
;;   (irc-connect ""))                     ; FIXME
