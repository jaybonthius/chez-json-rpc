(import (chezscheme) (json-rpc framing) (check))

(define (bytes s) (string->utf8 s))

(define (frame-of s) (body->frame (bytes s)))

(define (join . pieces)
  (call-with-bytevector-output-port
    (lambda (p) (for-each (lambda (b) (put-bytevector p b)) pieces))))

(define (pieces-of bv size)
  (let loop ([start 0] [out '()])
    (if (>= start (bytevector-length bv))
        (reverse out)
        (let ([end (min (bytevector-length bv) (+ start size))])
          (loop end (cons (bytevector-copy-range bv start end) out))))))

(define (bytevector-copy-range bv start end)
  (let ([copy (make-bytevector (- end start))])
    (bytevector-copy! bv start copy 0 (- end start))
    copy))

(define (read-pieces pieces)
  (let* ([skipped '()]
         [r (make-frame-reader (lambda (b) (set! skipped (cons b skipped))))]
         [out (read-in-order r pieces)])
    (values out (apply join (reverse skipped)))))

;; map may call its procedure in any order, and the pieces must be read in
;; turn.
(define (read-in-order r pieces)
  (let loop ([pieces pieces] [out '()])
    (if (null? pieces)
        (reverse out)
        (loop (cdr pieces) (append (reverse (frame-reader-read! r (car pieces))) out)))))

(define (texts out)
  (map (lambda (x) (if (bytevector? x) (utf8->string x) 'frame-error)) out))

(test "body->frame writes a Content-Length header and the body"
  (check-equal? (bytes "Content-Length: 5\r\n\r\nhello") (frame-of "hello"))
  (check-equal? (bytes "Content-Length: 0\r\n\r\n") (frame-of "")))

(test "a frame split across pieces, at every size, gives its body once"
  (let ([f (frame-of "{\"jsonrpc\":\"2.0\",\"method\":\"initialized\"}")])
    (for-each
      (lambda (size)
        (let-values ([(out skipped) (read-pieces (pieces-of f size))])
          (check-equal? '("{\"jsonrpc\":\"2.0\",\"method\":\"initialized\"}") (texts out))
          (check-equal? (bytevector) skipped)))
      '(1 2 3 7 16 21 1000))))

(test "two frames in one piece give both bodies, oldest first"
  (let-values ([(out skipped) (read-pieces (list (join (frame-of "one") (frame-of "two"))))])
    (check-equal? '("one" "two") (texts out))))

(test "an empty body is a body"
  (let-values ([(out skipped) (read-pieces (list (join (frame-of "") (frame-of "x"))))])
    (check-equal? '("" "x") (texts out))))

(test "a Content-Type line before Content-Length goes to skipped, and the frame is read"
  (let-values ([(out skipped)
                (read-pieces
                  (list (bytes "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\nContent-Length: 2\r\n\r\nhi")))])
    (check-equal? '("hi") (texts out))
    (check-equal? (bytes "Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n") skipped)))

(test "headers after Content-Length other than Content-Length are ignored"
  (let-values ([(out skipped)
                (read-pieces (list (bytes "Content-Length: 2\r\nContent-Type: text/plain\r\n\r\nhi")))])
    (check-equal? '("hi") (texts out))
    (check-equal? (bytevector) skipped)))

(test "Content-Length is matched whatever its case, and a line ending in LF alone ends it"
  (let-values ([(out skipped) (read-pieces (list (bytes "content-length:3\n\nabc")))])
    (check-equal? '("abc") (texts out))))

(test "a body of 5 MB is read, in pieces of 64 KB"
  (let* ([body (make-bytevector (* 5 1024 1024) 120)]
         [f (body->frame body)])
    (let-values ([(out skipped) (read-pieces (pieces-of f 65536))])
      (check-equal? 1 (length out))
      (check (bytevector=? body (car out))))))

(test "a stray line of 10 KB before a header goes to skipped in pieces as it arrives"
  (let* ([line (join (make-bytevector 10240 (char->integer #\x)) (bytes "\n"))]
         [calls '()]
         [r (make-frame-reader (lambda (b) (set! calls (cons b calls))))])
    (for-each
      (lambda (piece)
        (let ([before (length calls)])
          (check-equal? '() (frame-reader-read! r piece))
          (check-equal? (+ before 1) (length calls))
          (check-equal? piece (car calls))))
      (pieces-of line 1024))
    (check-equal? line (apply join (reverse calls)))
    (check-equal? '("ok") (texts (frame-reader-read! r (frame-of "ok"))))))

(test "stray output that starts like a header but is not one goes to skipped"
  (let-values ([(out skipped)
                (read-pieces (list (bytes "Content-Lengthy\r\nContent: x\r\n") (frame-of "a")))])
    (check-equal? '("a") (texts out))
    (check-equal? (bytes "Content-Lengthy\r\nContent: x\r\n") skipped)))

(test "a header line over 8,192 bytes between two valid frames gives body, error, body, from one call"
  (let* ([long (make-bytevector 9000 (char->integer #\a))]
         [bad (join (bytes "Content-Length: 3\r\nX-Long: ") long (bytes "\r\n\r\nbad"))]
         [r (make-frame-reader)]
         [out (frame-reader-read! r (join (frame-of "first") bad (frame-of "second")))])
    (check-equal? 3 (length out))
    (check-equal? "first" (utf8->string (car out)))
    (check (frame-error? (cadr out)))
    (check-equal? "second" (utf8->string (caddr out)))))

(test "a body longer than frame-max-length between two valid frames gives body, error, body"
  (let* ([r (parameterize ([frame-max-length 10]) (make-frame-reader))]
         [out (frame-reader-read! r (join (frame-of "first") (frame-of "this body is too long") (frame-of "second")))])
    (check-equal? '("first" frame-error "second") (texts out))
    (check (frame-error? (cadr out)))))

(test "an overlong Content-Length line, split across pieces, leaves the length unknown: its body is stray"
  (let ([stream (join (bytes "Content-Length: ") (make-bytevector 9000 (char->integer #\0)) (bytes "4\r\n\r\nbody\n")
                      (frame-of "next"))])
    (let-values ([(out skipped) (read-pieces (pieces-of stream 100))])
      (check-equal? '(frame-error "next") (texts out))
      (check-equal? (bytes "body\n") skipped))))

(test "a bad body split across pieces is dropped as it arrives, and the next frame read"
  (let* ([r (parameterize ([frame-max-length 10]) (make-frame-reader))]
         [stream (join (frame-of "this body is too long") (frame-of "next"))]
         [out (read-in-order r (pieces-of stream 4))])
    (check-equal? '(frame-error "next") (texts out))))

(test "a Content-Length that is not a byte count, and a second Content-Length, are frame errors"
  (let-values ([(out skipped)
                (read-pieces (list (bytes "Content-Length: many\r\n\r\n") (frame-of "a")
                                   (bytes "Content-Length: 1\r\nContent-Length: 1\r\n\r\nz")
                                   (frame-of "b")))])
    (check-equal? '(frame-error "a" frame-error "b") (texts out))))

(test "a Content-Length too large for a fixnum is a frame error, and its body is dropped"
  (let-values ([(out skipped)
                (read-pieces (list (bytes "Content-Length: 100000000000000000000000000000\r\n\r\nxyz")))])
    (check-equal? '(frame-error) (texts out))
    (check-equal? (bytevector) skipped)))

(test "frame-reader-read! never changes the bytes it is given"
  (let* ([f (join (frame-of "one") (bytes "noise\n") (frame-of "two"))]
         [copy (bytevector-copy f)])
    (frame-reader-read! (make-frame-reader) f)
    (check-equal? copy f)))

(test "frame-max-length is read when the reader is made"
  (let ([r (parameterize ([frame-max-length 3]) (make-frame-reader))])
    (check-equal? '(frame-error) (texts (frame-reader-read! r (frame-of "four"))))
    (check-equal? '("abc") (texts (frame-reader-read! (make-frame-reader) (frame-of "abc"))))))

(test "bad arguments raise"
  (check-raises assertion-violation? (frame-reader-read! 'reader (bytevector)))
  (check-raises assertion-violation? (frame-reader-read! (make-frame-reader) "text"))
  (check-raises assertion-violation? (make-frame-reader 'skip))
  (check-raises assertion-violation? (body->frame "text"))
  (check-raises assertion-violation? (parameterize ([frame-max-length 0]) #f))
  (check-raises assertion-violation? (read-frame (open-string-input-port "x"))))

(define (port-of . pieces) (open-bytevector-input-port (apply join pieces)))

(test "read-frame reads one body at a time from a port, by the same rules"
  (let* ([skipped '()]
         [keep (lambda (b) (set! skipped (cons b skipped)))]
         [p (port-of (bytes "Starting server\n")
                     (bytes "Content-Type: x\r\nContent-Length: 3\r\n\r\none")
                     (frame-of "two"))])
    (check-equal? "one" (utf8->string (read-frame p keep)))
    (check-equal? "two" (utf8->string (read-frame p keep)))
    (check (eof-object? (read-frame p keep)))
    (check-equal? (bytes "Starting server\nContent-Type: x\r\n") (apply join (reverse skipped)))))

(test "read-frame reads no further than its frame"
  (let ([p (port-of (frame-of "one") (bytes "rest"))])
    (check-equal? "one" (utf8->string (read-frame p)))
    (check-equal? (bytes "rest") (get-bytevector-all p))))

(test "read-frame raises after consuming a bad frame, and its next call reads the frame after"
  (let ([p (parameterize ([frame-max-length 5])
             (port-of (frame-of "first") (frame-of "too long") (frame-of "next")))])
    (parameterize ([frame-max-length 5])
      (check-equal? "first" (utf8->string (read-frame p)))
      (check-raises frame-error? (read-frame p))
      (check-equal? "next" (utf8->string (read-frame p))))))

(test "read-frame raises after an overlong header line and reads on"
  (let ([p (port-of (bytes "Content-Length: 3\r\nX: ") (make-bytevector 9000 97) (bytes "\r\n\r\nbad")
                    (frame-of "next"))])
    (check-raises frame-error? (read-frame p))
    (check-equal? "next" (utf8->string (read-frame p)))))

(test "read-frame raises when the input ends inside a frame"
  (check-raises frame-error? (read-frame (port-of (bytes "Content-Length: 9\r\n\r\nshort"))))
  (check-raises frame-error? (read-frame (port-of (bytes "Content-Length: 9\r\n"))))
  (check (eof-object? (read-frame (port-of (bytes "trailing noise without a newline"))))))

(test-exit)
