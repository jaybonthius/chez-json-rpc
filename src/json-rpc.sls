(library (json-rpc)
  (export
    make-json-rpc-request json-rpc-request? json-rpc-request-id json-rpc-request-method
    json-rpc-request-params
    make-json-rpc-notification json-rpc-notification? json-rpc-notification-method
    json-rpc-notification-params
    make-json-rpc-response make-json-rpc-error-response json-rpc-response?
    json-rpc-response-id json-rpc-response-result json-rpc-response-error
    json->json-rpc-message bytevector->json-rpc-message json-rpc-message->bytevector
    &json-rpc-error make-json-rpc-error json-rpc-error? json-rpc-error-code json-rpc-error-data
    json-rpc-parse-error-code json-rpc-invalid-request-code json-rpc-method-not-found-code
    json-rpc-invalid-params-code json-rpc-internal-error-code)
  (import (chezscheme) (json))

  (define json-rpc-parse-error-code -32700)
  (define json-rpc-invalid-request-code -32600)
  (define json-rpc-method-not-found-code -32601)
  (define json-rpc-invalid-params-code -32602)
  (define json-rpc-internal-error-code -32603)

  (define-condition-type &json-rpc-error &error make-error-condition json-rpc-error?
    (code json-rpc-error-code)
    (data json-rpc-error-data))

  (define make-json-rpc-error
    (case-lambda
      [(code message) (make-json-rpc-error code message 'null)]
      [(code message data)
       (unless (and (integer? code) (exact? code))
         (assertion-violationf 'make-json-rpc-error "~s is not an exact integer" code))
       (unless (string? message)
         (assertion-violationf 'make-json-rpc-error "~s is not a string" message))
       (condition (make-error-condition code data) (make-message-condition message))]))

  (define (id? v) (or (and (integer? v) (exact? v)) (string? v)))

  ;; The empty object reads as (), so an object is any list of pairs.
  (define (object? v) (and (list? v) (for-all pair? v)))

  (define (params? v) (or (eq? v 'null) (vector? v) (object? v)))

  (define (check-id who id)
    (unless (id? id)
      (assertion-violationf who "~s is not an exact integer or a string" id)))

  (define (check-method who method)
    (unless (string? method) (assertion-violationf who "~s is not a string" method)))

  (define (check-params who params)
    (unless (params? params)
      (assertion-violationf who "~s is not an object, an array or null" params)))

  (define-record-type (json-rpc-request new-request json-rpc-request?)
    (sealed #t)
    (fields id method params))

  (define-record-type (json-rpc-notification new-notification json-rpc-notification?)
    (sealed #t)
    (fields method params))

  (define-record-type (json-rpc-response new-response json-rpc-response?)
    (sealed #t)
    (fields (immutable id json-rpc-response-id)
            (immutable result response-result)
            (immutable error json-rpc-response-error)))

  (define make-json-rpc-request
    (case-lambda
      [(id method) (make-json-rpc-request id method 'null)]
      [(id method params)
       (check-id 'make-json-rpc-request id)
       (check-method 'make-json-rpc-request method)
       (check-params 'make-json-rpc-request params)
       (new-request id method params)]))

  (define make-json-rpc-notification
    (case-lambda
      [(method) (make-json-rpc-notification method 'null)]
      [(method params)
       (check-method 'make-json-rpc-notification method)
       (check-params 'make-json-rpc-notification params)
       (new-notification method params)]))

  ;; A response to a request whose id could not be read has the id null.
  (define (check-response-id who id)
    (unless (or (id? id) (eq? id 'null))
      (assertion-violationf who "~s is not an exact integer, a string or null" id)))

  (define (make-json-rpc-response id result)
    (check-response-id 'make-json-rpc-response id)
    (new-response id result #f))

  (define (make-json-rpc-error-response id condition)
    (check-response-id 'make-json-rpc-error-response id)
    (unless (json-rpc-error? condition)
      (assertion-violationf 'make-json-rpc-error-response "~s is not a json-rpc error" condition))
    (new-response id #f condition))

  (define (json-rpc-response-result response)
    (unless (json-rpc-response? response)
      (assertion-violationf 'json-rpc-response-result "~s is not a response" response))
    (when (json-rpc-response-error response)
      (assertion-violation 'json-rpc-response-result "an error response has no result" response))
    (response-result response))

  (define (invalid who fmt . args)
    (raise (condition (make-error-condition json-rpc-invalid-request-code 'null)
             (make-who-condition who) (make-message-condition (apply format fmt args)))))

  (define (member-ref object key)
    (let ([m (assoc key object)]) (if m (cdr m) (void))))

  (define (present? v) (not (eq? v (void))))

  (define (read-error who value)
    (unless (object? value)
      (invalid who "the error ~s is not an object" value))
    (let ([code (member-ref value "code")]
          [message (member-ref value "message")]
          [data (member-ref value "data")])
      (unless (and (integer? code) (exact? code))
        (invalid who "the error code ~s is not an integer" code))
      (unless (string? message)
        (invalid who "the error message ~s is not a string" message))
      (make-json-rpc-error code message (if (present? data) data 'null))))

  (define (message-from-json who value)
    (cond
      [(vector? value) (invalid who "a batch is not read")]
      [(not (object? value)) (invalid who "~s is not an object" value)]
      [else
       (let ([version (member-ref value "jsonrpc")]
             [id (member-ref value "id")]
             [method (member-ref value "method")]
             [params (member-ref value "params")]
             [result (member-ref value "result")]
             [error-value (member-ref value "error")])
         (unless (equal? version "2.0")
           (invalid who "jsonrpc is ~s, not \"2.0\"" (if (present? version) version 'absent)))
         (cond
           [(present? method)
            (unless (string? method) (invalid who "the method ~s is not a string" method))
            (when (or (present? result) (present? error-value))
              (invalid who "a request has a result or an error"))
            (let ([params (if (present? params) params 'null)])
              (unless (params? params)
                (invalid who "the params ~s are not an object or an array" params))
              (cond
                [(not (present? id)) (new-notification method params)]
                [(id? id) (new-request id method params)]
                [else (invalid who "the request id ~s is not an integer or a string" id)]))]
           [(or (present? result) (present? error-value))
            (when (and (present? result) (present? error-value))
              (invalid who "a response has both a result and an error"))
            (unless (and (present? id) (or (id? id) (eq? id 'null)))
              (invalid who "the response id ~s is not an integer, a string or null"
                (if (present? id) id 'absent)))
            (if (present? error-value)
                (new-response id #f (read-error who error-value))
                (new-response id result #f))]
           [else (invalid who "the object has no method, result or error")]))]))

  (define (json->json-rpc-message value)
    (message-from-json 'json->json-rpc-message value))

  (define (bytevector->json-rpc-message body)
    (unless (bytevector? body)
      (assertion-violationf 'bytevector->json-rpc-message "~s is not a bytevector" body))
    (message-from-json 'bytevector->json-rpc-message
      (guard (c [(json-error? c)
                 (raise (condition (make-error-condition json-rpc-parse-error-code 'null)
                          (make-who-condition 'bytevector->json-rpc-message)
                          (make-message-condition
                            (if (message-condition? c) (condition-message c) "not JSON"))))])
        (bytevector->json body))))

  (define (with-params fields params)
    (if (eq? params 'null) fields (append fields (list (cons "params" params)))))

  (define (error->json c)
    (let ([fields (list (cons "code" (json-rpc-error-code c))
                        (cons "message" (if (message-condition? c) (condition-message c) "")))])
      (if (eq? (json-rpc-error-data c) 'null)
          fields
          (append fields (list (cons "data" (json-rpc-error-data c)))))))

  (define (json-rpc-message->bytevector message)
    (json->bytevector
      (cond
        [(json-rpc-request? message)
         (with-params
           (list '("jsonrpc" . "2.0") (cons "id" (json-rpc-request-id message))
                 (cons "method" (json-rpc-request-method message)))
           (json-rpc-request-params message))]
        [(json-rpc-notification? message)
         (with-params
           (list '("jsonrpc" . "2.0") (cons "method" (json-rpc-notification-method message)))
           (json-rpc-notification-params message))]
        [(json-rpc-response? message)
         (list '("jsonrpc" . "2.0") (cons "id" (json-rpc-response-id message))
               (let ([c (json-rpc-response-error message)])
                 (if c
                     (cons "error" (error->json c))
                     (cons "result" (response-result message)))))]
        [else
         (assertion-violationf 'json-rpc-message->bytevector "~s is not a message" message)]))))
