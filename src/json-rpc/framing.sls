(library (json-rpc framing)
  (export
    ;; Reading
    make-frame-reader frame-reader? frame-reader-read! read-frame
    ;; Writing
    body->frame
    ;; Limits and errors
    frame-max-length &frame-error frame-error?)
  (import (chezscheme))

  (define frame-max-length
    (make-thread-parameter (* 256 1024 1024)
      (lambda (n)
        (unless (and (fixnum? n) (fx> n 0))
          (assertion-violationf 'frame-max-length "~s is not a positive fixnum" n))
        n)))

  (define-condition-type &frame-error &error make-frame-error frame-error?)

  ;; A peer that sends a header without end must not make the reader buffer
  ;; without bound.
  (define max-header-line 8192)

  ;; A line's last byte may be the CR of its CRLF, so one byte more than the
  ;; limit is held before the line is known to be too long.
  (define line-capacity (fx+ max-header-line 1))

  (define content-length-prefix (string->utf8 "content-length:"))
  (define prefix-length (bytevector-length content-length-prefix))

  ;; read-frame passes a stray line without end to skipped in parts of this
  ;; size rather than hold it.
  (define stray-part 4096)

  (define lf 10)
  (define cr 13)

  ;; A header is recognised wherever it starts, not only at a line's start,
  ;; so stray output without a line end, or a body whose length was miscounted,
  ;; costs the frames it overlaps and not every frame after it.
  (define-record-type (frame-reader new-frame-reader frame-reader?)
    (sealed #t)
    (fields
      (immutable skipped)
      (immutable max-length)
      (mutable state)
      (mutable match)
      (immutable line)
      (mutable line-length)
      (mutable length)
      (mutable body)
      (mutable remaining)
      (mutable stray)
      (mutable stray-count)
      (mutable line-blank?))
    (protocol
      (lambda (new)
        (lambda (skipped max-length)
          (new skipped max-length 'stray 0 (make-bytevector line-capacity) 0 #f #f 0 #f 0 #t)))))

  (define make-frame-reader
    (case-lambda
      [() (make-frame-reader ignore-bytes)]
      [(skipped)
       (unless (procedure? skipped)
         (assertion-violationf 'make-frame-reader "~s is not a procedure" skipped))
       (new-frame-reader skipped (frame-max-length))]))

  (define (ignore-bytes bytes) (void))

  (define (frame-error who fmt . args)
    (condition (make-frame-error) (make-who-condition who)
      (make-message-condition (apply format fmt args))))

  (define (ascii-downcase b)
    (if (fx<= 65 b 90) (fx+ b 32) b))

  (define (stray-put! r bytes start end)
    (unless (fx= start end)
      (unless (frame-reader-stray r)
        (let-values ([(port extract) (open-bytevector-output-port)])
          (frame-reader-stray-set! r (cons port extract))))
      (put-bytevector (car (frame-reader-stray r)) bytes start (fx- end start))
      (frame-reader-stray-count-set! r (fx+ (frame-reader-stray-count r) (fx- end start)))))

  (define (flush-stray! r)
    (let ([stray (frame-reader-stray r)])
      (when stray
        (frame-reader-stray-set! r #f)
        (frame-reader-stray-count-set! r 0)
        ((frame-reader-skipped r) ((cdr stray))))))

  (define (find-prefix-start bytes start end)
    (let loop ([i start])
      (cond
        [(fx= i end) #f]
        [(fx= (ascii-downcase (bytevector-u8-ref bytes i)) (bytevector-u8-ref content-length-prefix 0)) i]
        [else (loop (fx+ i 1))])))

  (define (find-lf bytes start end)
    (let loop ([i start])
      (cond
        [(fx= i end) #f]
        [(fx= (bytevector-u8-ref bytes i) lf) i]
        [else (loop (fx+ i 1))])))

  (define (to-stray! r)
    (frame-reader-state-set! r 'stray)
    (frame-reader-match-set! r 0)
    (frame-reader-line-length-set! r 0)
    (frame-reader-length-set! r #f)
    (frame-reader-body-set! r #f))

  (define (discard-header! r at-line-start?)
    (frame-reader-state-set! r 'discard-header)
    (frame-reader-line-blank?-set! r at-line-start?))

  (define (blank? b) (or (fx= b 32) (fx= b 9)))

  (define (content-length line end)
    (let loop ([start prefix-length] [end end])
      (cond
        [(and (fx< start end) (blank? (bytevector-u8-ref line start))) (loop (fx+ start 1) end)]
        [(and (fx< start end) (blank? (bytevector-u8-ref line (fx- end 1)))) (loop start (fx- end 1))]
        [(fx= start end) #f]
        [else
         (let digits ([i start] [n 0])
           (if (fx= i end)
               n
               (let ([b (bytevector-u8-ref line i)])
                 (and (fx<= 48 b 57) (digits (fx+ i 1) (+ (* n 10) (fx- b 48)))))))])))

  (define (starts-with-content-length? line end)
    (and (fx>= end prefix-length)
         (let loop ([i 0])
           (or (fx= i prefix-length)
               (and (fx= (ascii-downcase (bytevector-u8-ref line i))
                         (bytevector-u8-ref content-length-prefix i))
                    (loop (fx+ i 1)))))))

  (define (end-header-block! r who emit)
    (let ([n (frame-reader-length r)])
      (cond
        [(= n 0) (to-stray! r) (emit (make-bytevector 0))]
        [else
         (frame-reader-state-set! r 'body)
         (frame-reader-body-set! r (make-bytevector n))
         (frame-reader-remaining-set! r n)])))

  (define (header-line! r who emit)
    (let* ([line (frame-reader-line r)]
           [n (frame-reader-line-length r)]
           [end (if (and (fx> n 0) (fx= (bytevector-u8-ref line (fx- n 1)) cr)) (fx- n 1) n)])
      (frame-reader-line-length-set! r 0)
      (cond
        [(fx> end max-header-line)
         (emit (frame-error who "header line longer than ~s bytes" max-header-line))
         (discard-header! r #t)]
        [(fx= end 0) (end-header-block! r who emit)]
        [(not (frame-reader-length r))
         (let ([count (content-length line end)])
           (cond
             [(not count)
              (emit (frame-error who "Content-Length ~s is not a byte count"
                      (utf8->string (bytevector-copy-range line prefix-length end))))
              (discard-header! r #t)]
             ;; A length the reader will not hold is no length: dropping that
             ;; many bytes would drop every later frame when it is not honest,
             ;; so the body is read as stray and the next header found.
             [(> count (frame-reader-max-length r))
              (emit (frame-error who "frame of ~s bytes is longer than frame-max-length, ~s"
                      count (frame-reader-max-length r)))
              (discard-header! r #t)]
             [else (frame-reader-length-set! r count)]))]
        [(starts-with-content-length? line end)
         (emit (frame-error who "more than one Content-Length header"))
         (frame-reader-length-set! r #f)
         (discard-header! r #t)])))

  (define (bytevector-copy-range bytes start end)
    (let ([copy (make-bytevector (fx- end start))])
      (bytevector-copy! bytes start copy 0 (fx- end start))
      copy))

  ;; read-frame feeds one byte at a time, so it passes flush? #f and flushes
  ;; at line ends, or skipped would be called once per byte.
  (define (feed! r bytes start end who emit flush?)
    (let loop ([i start])
      (when (fx< i end)
        (case (frame-reader-state r)
          [(stray)
           (let ([m (frame-reader-match r)])
             (if (fx= m 0)
                 (let ([c-at (find-prefix-start bytes i end)])
                   (stray-put! r bytes i (or c-at end))
                   (when c-at
                     (bytevector-u8-set! (frame-reader-line r) 0 (bytevector-u8-ref bytes c-at))
                     (frame-reader-match-set! r 1)
                     (loop (fx+ c-at 1))))
                 (let ([b (bytevector-u8-ref bytes i)])
                   (cond
                     [(fx= (ascii-downcase b) (bytevector-u8-ref content-length-prefix m))
                      (bytevector-u8-set! (frame-reader-line r) m b)
                      (if (fx= (fx+ m 1) prefix-length)
                          (begin
                            (flush-stray! r)
                            (frame-reader-state-set! r 'header)
                            (frame-reader-line-length-set! r prefix-length)
                            (frame-reader-match-set! r 0))
                          (frame-reader-match-set! r (fx+ m 1)))
                      (loop (fx+ i 1))]
                     [else
                      ;; No letter of the prefix after its first starts it
                      ;; again, so the failing byte is the only one that can.
                      (stray-put! r (frame-reader-line r) 0 m)
                      (frame-reader-match-set! r 0)
                      (loop i)]))))]
          [(header)
           (let* ([lf-at (find-lf bytes i end)]
                  [stop (or lf-at end)]
                  [line (frame-reader-line r)]
                  [held (frame-reader-line-length r)]
                  [room (fx- line-capacity held)])
             (if (fx> (fx- stop i) room)
                 (begin
                   (emit (frame-error who "header line longer than ~s bytes" max-header-line))
                   (discard-header! r #f)
                   (loop (fx+ i room)))
                 (begin
                   (bytevector-copy! bytes i line held (fx- stop i))
                   (frame-reader-line-length-set! r (fx+ held (fx- stop i)))
                   (when lf-at
                     (header-line! r who emit)
                     (loop (fx+ lf-at 1))))))]
          [(discard-header)
           (let ([b (bytevector-u8-ref bytes i)])
             (cond
               [(fx= b lf)
                (if (frame-reader-line-blank? r)
                    (let ([count (frame-reader-length r)])
                      (if (and count (> count 0))
                          (begin
                            (frame-reader-state-set! r 'discard-body)
                            (frame-reader-remaining-set! r count))
                          (to-stray! r)))
                    (frame-reader-line-blank?-set! r #t))]
               [(not (fx= b cr)) (frame-reader-line-blank?-set! r #f)])
             (loop (fx+ i 1)))]
          [(body)
           (let* ([body (frame-reader-body r)]
                  [remaining (frame-reader-remaining r)]
                  [n (fxmin remaining (fx- end i))])
             (bytevector-copy! bytes i body (fx- (bytevector-length body) remaining) n)
             (if (fx= n remaining)
                 (begin (to-stray! r) (emit body))
                 (frame-reader-remaining-set! r (fx- remaining n)))
             (loop (fx+ i n)))]
          [(discard-body)
           (let* ([remaining (frame-reader-remaining r)]
                  [n (min remaining (fx- end i))])
             (if (= n remaining)
                 (to-stray! r)
                 (frame-reader-remaining-set! r (- remaining n)))
             (loop (fx+ i n)))])))
    (when flush? (flush-stray! r)))

  (define (frame-reader-read! r bytes)
    (unless (frame-reader? r)
      (assertion-violationf 'frame-reader-read! "~s is not a frame reader" r))
    (unless (bytevector? bytes)
      (assertion-violationf 'frame-reader-read! "~s is not a bytevector" bytes))
    (let ([out '()])
      (feed! r bytes 0 (bytevector-length bytes) 'frame-reader-read!
        (lambda (x) (set! out (cons x out)))
        #t)
      (reverse out)))

  ;; Reads byte by byte outside bodies, so the port is never read past the
  ;; frame and the next call starts where this one stopped.
  (define read-frame
    (case-lambda
      [(port) (read-frame port ignore-bytes)]
      [(port skipped)
       (unless (and (input-port? port) (binary-port? port))
         (assertion-violationf 'read-frame "~s is not a binary input port" port))
       (unless (procedure? skipped)
         (assertion-violationf 'read-frame "~s is not a procedure" skipped))
       (let ([r (new-frame-reader skipped (frame-max-length))]
             [one (make-bytevector 1)]
             [body #f]
             [failure #f])
         (define (emit x) (if (bytevector? x) (set! body x) (set! failure (or failure x))))
         (define (finished?)
           (or body (and failure (eq? (frame-reader-state r) 'stray))))
         (define (end-of-input)
           (if (and (eq? (frame-reader-state r) 'stray) (not failure))
               (begin
                 (stray-put! r (frame-reader-line r) 0 (frame-reader-match r))
                 (flush-stray! r)
                 (eof-object))
               (begin
                 (flush-stray! r)
                 (raise (or failure (frame-error 'read-frame "end of input inside a frame"))))))
         (let loop ()
           (case (frame-reader-state r)
             [(body discard-body)
              (let ([chunk (get-bytevector-n port
                             (if (eq? (frame-reader-state r) 'body)
                                 (frame-reader-remaining r)
                                 (min (frame-reader-remaining r) 65536)))])
                (if (eof-object? chunk)
                    (end-of-input)
                    (begin
                      (feed! r chunk 0 (bytevector-length chunk) 'read-frame emit #f)
                      (if (finished?) (finish r body failure) (loop)))))]
             [else
              (let ([b (get-u8 port)])
                (if (eof-object? b)
                    (end-of-input)
                    (begin
                      (bytevector-u8-set! one 0 b)
                      (feed! r one 0 1 'read-frame emit #f)
                      (when (or (fx= b lf) (fx>= (frame-reader-stray-count r) stray-part))
                        (flush-stray! r))
                      (if (finished?) (finish r body failure) (loop)))))])))]))

  (define (finish r body failure)
    (flush-stray! r)
    (or body (raise failure)))

  (define (body->frame body)
    (unless (bytevector? body)
      (assertion-violationf 'body->frame "~s is not a bytevector" body))
    (let* ([header (string->utf8 (format "Content-Length: ~d\r\n\r\n" (bytevector-length body)))]
           [frame (make-bytevector (fx+ (bytevector-length header) (bytevector-length body)))])
      (bytevector-copy! header 0 frame 0 (bytevector-length header))
      (bytevector-copy! body 0 frame (bytevector-length header) (bytevector-length body))
      frame)))
