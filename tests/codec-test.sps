(import (chezscheme) (json) (json-rpc) (check))

(define (read-text s) (bytevector->json-rpc-message (string->utf8 s)))

(define (written message) (utf8->string (json-rpc-message->bytevector message)))

(define (error-code thunk)
  (guard (c [(json-rpc-error? c) (json-rpc-error-code c)]) (thunk) 'no-error))

;; The examples are the specification's own, from its section 7.
(test "a request with positional parameters"
  (let ([m (read-text "{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": [42, 23], \"id\": 1}")])
    (check (json-rpc-request? m))
    (check-equal? 1 (json-rpc-request-id m))
    (check-equal? "subtract" (json-rpc-request-method m))
    (check-equal? '#(42 23) (json-rpc-request-params m))))

(test "a request with named parameters and a string id"
  (let ([m (read-text "{\"jsonrpc\": \"2.0\", \"method\": \"subtract\", \"params\": {\"minuend\": 42, \"subtrahend\": 23}, \"id\": \"3\"}")])
    (check-equal? "3" (json-rpc-request-id m))
    (check-equal? '(("minuend" . 42) ("subtrahend" . 23)) (json-rpc-request-params m))))

(test "a notification has no id"
  (let ([m (read-text "{\"jsonrpc\": \"2.0\", \"method\": \"update\", \"params\": [1,2,3,4,5]}")])
    (check (json-rpc-notification? m))
    (check-equal? "update" (json-rpc-notification-method m))
    (check-equal? '#(1 2 3 4 5) (json-rpc-notification-params m)))
  (let ([m (read-text "{\"jsonrpc\": \"2.0\", \"method\": \"foobar\"}")])
    (check (json-rpc-notification? m))
    (check-equal? 'null (json-rpc-notification-params m))))

(test "absent and null params read as null"
  (check-equal? 'null (json-rpc-request-params (read-text "{\"jsonrpc\":\"2.0\",\"method\":\"m\",\"id\":1}")))
  (check-equal? 'null (json-rpc-request-params (read-text "{\"jsonrpc\":\"2.0\",\"method\":\"m\",\"params\":null,\"id\":1}"))))

(test "a response with a result"
  (let ([m (read-text "{\"jsonrpc\": \"2.0\", \"result\": 19, \"id\": 1}")])
    (check (json-rpc-response? m))
    (check-equal? 1 (json-rpc-response-id m))
    (check-equal? 19 (json-rpc-response-result m))
    (check-equal? #f (json-rpc-response-error m)))
  (check-equal? #f (json-rpc-response-result (read-text "{\"jsonrpc\":\"2.0\",\"result\":false,\"id\":2}")))
  (check-equal? 'null (json-rpc-response-result (read-text "{\"jsonrpc\":\"2.0\",\"result\":null,\"id\":2}"))))

(test "an error response holds its condition, and has no result"
  (let* ([m (read-text "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32601, \"message\": \"Method not found\"}, \"id\": \"1\"}")]
         [c (json-rpc-response-error m)])
    (check (json-rpc-error? c))
    (check-equal? json-rpc-method-not-found-code (json-rpc-error-code c))
    (check-equal? "Method not found" (condition-message c))
    (check-equal? 'null (json-rpc-error-data c))
    (check-raises assertion-violation? (json-rpc-response-result m)))
  (let ([c (json-rpc-response-error
             (read-text "{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32000,\"message\":\"m\",\"data\":{\"why\":\"x\"}},\"id\":null}"))])
    (check-equal? -32000 (json-rpc-error-code c))
    (check-equal? '(("why" . "x")) (json-rpc-error-data c))))

(test "a response to an unreadable request has the id null"
  (check-equal? 'null (json-rpc-response-id (read-text "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": -32700, \"message\": \"Parse error\"}, \"id\": null}"))))

(test "a body that is not JSON raises the parse error code"
  (check-equal? json-rpc-parse-error-code
    (error-code (lambda () (read-text "{\"jsonrpc\": \"2.0\", \"method\": \"foobar, \"params\": \"bar\", \"baz]")))))

(test "what is not one message raises the invalid request code"
  (for-each
    (lambda (text)
      (check-equal? json-rpc-invalid-request-code (error-code (lambda () (read-text text)))))
    (list "{\"jsonrpc\": \"2.0\", \"method\": 1, \"params\": \"bar\"}"
          "[{\"jsonrpc\": \"2.0\", \"method\": \"sum\", \"params\": [1,2,4], \"id\": \"1\"}]"
          "[]"
          "[1,2,3]"
          "42"
          "{\"method\": \"m\", \"id\": 1}"
          "{\"jsonrpc\": \"1.0\", \"method\": \"m\", \"id\": 1}"
          "{\"jsonrpc\": \"2.0\", \"method\": \"m\", \"params\": 3, \"id\": 1}"
          "{\"jsonrpc\": \"2.0\", \"method\": \"m\", \"id\": 1.5}"
          "{\"jsonrpc\": \"2.0\", \"method\": \"m\", \"id\": null}"
          "{\"jsonrpc\": \"2.0\", \"method\": \"m\", \"result\": 1, \"id\": 1}"
          "{\"jsonrpc\": \"2.0\", \"result\": 1, \"error\": {\"code\": 1, \"message\": \"m\"}, \"id\": 1}"
          "{\"jsonrpc\": \"2.0\", \"result\": 1}"
          "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": \"x\", \"message\": \"m\"}, \"id\": 1}"
          "{\"jsonrpc\": \"2.0\", \"error\": {\"code\": 1}, \"id\": 1}"
          "{\"jsonrpc\": \"2.0\", \"id\": 1}")))

(test "json->json-rpc-message reads a value (json) parsed"
  (let ([m (json->json-rpc-message (bytevector->json (string->utf8 "{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}")))])
    (check (json-rpc-notification? m))
    (check-equal? "exit" (json-rpc-notification-method m)))
  (check-equal? json-rpc-invalid-request-code (error-code (lambda () (json->json-rpc-message "text")))))

(test "messages are written as the specification writes them, null params left out"
  (check-equal? "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"subtract\",\"params\":[42,23]}"
    (written (make-json-rpc-request 1 "subtract" '#(42 23))))
  (check-equal? "{\"jsonrpc\":\"2.0\",\"id\":\"a\",\"method\":\"shutdown\"}"
    (written (make-json-rpc-request "a" "shutdown")))
  (check-equal? "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"m\",\"params\":{}}"
    (written (make-json-rpc-request 2 "m" '())))
  (check-equal? "{\"jsonrpc\":\"2.0\",\"method\":\"update\",\"params\":[1,2]}"
    (written (make-json-rpc-notification "update" '#(1 2))))
  (check-equal? "{\"jsonrpc\":\"2.0\",\"method\":\"exit\"}"
    (written (make-json-rpc-notification "exit")))
  (check-equal? "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":19}"
    (written (make-json-rpc-response 1 19)))
  (check-equal? "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":null}"
    (written (make-json-rpc-response 1 'null)))
  (check-equal? "{\"jsonrpc\":\"2.0\",\"id\":null,\"error\":{\"code\":-32700,\"message\":\"Parse error\"}}"
    (written (make-json-rpc-error-response 'null (make-json-rpc-error json-rpc-parse-error-code "Parse error"))))
  (check-equal? "{\"jsonrpc\":\"2.0\",\"id\":5,\"error\":{\"code\":-32602,\"message\":\"bad\",\"data\":[1]}}"
    (written (make-json-rpc-error-response 5 (make-json-rpc-error json-rpc-invalid-params-code "bad" '#(1))))))

(test "a written message reads back as the same message"
  (for-each
    (lambda (m)
      (check-equal? (written m) (written (bytevector->json-rpc-message (json-rpc-message->bytevector m)))))
    (list (make-json-rpc-request 7 "textDocument/hover" '(("position" ("line" . 1) ("character" . 4))))
          (make-json-rpc-notification "$/cancelRequest" '(("id" . 7)))
          (make-json-rpc-response "x" '#("a" #t #f null))
          (make-json-rpc-error-response 3 (make-json-rpc-error -32002 "Server not initialized")))))

(test "the five standard codes"
  (check-equal? '(-32700 -32600 -32601 -32602 -32603)
    (list json-rpc-parse-error-code json-rpc-invalid-request-code json-rpc-method-not-found-code
          json-rpc-invalid-params-code json-rpc-internal-error-code)))

(test "make-json-rpc-error makes a condition with a message, its data null by default"
  (let ([c (make-json-rpc-error json-rpc-internal-error-code "broke")])
    (check (json-rpc-error? c))
    (check (error? c))
    (check-equal? "broke" (condition-message c))
    (check-equal? 'null (json-rpc-error-data c))
    (check-equal? "broke" (guard (e [(json-rpc-error? e) (condition-message e)]) (raise c)))))

(test "constructors check their arguments"
  (check-raises assertion-violation? (make-json-rpc-request 1.5 "m"))
  (check-raises assertion-violation? (make-json-rpc-request 'null "m"))
  (check-raises assertion-violation? (make-json-rpc-request 1 'm))
  (check-raises assertion-violation? (make-json-rpc-request 1 "m" "params"))
  (check-raises assertion-violation? (make-json-rpc-notification "m" 3))
  (check-raises assertion-violation? (make-json-rpc-response 'other 1))
  (check-raises assertion-violation? (make-json-rpc-error-response 1 (make-error)))
  (check-raises assertion-violation? (make-json-rpc-error "code" "m"))
  (check-raises assertion-violation? (make-json-rpc-error 1 'm))
  (check-raises assertion-violation? (bytevector->json-rpc-message "text"))
  (check-raises assertion-violation? (json-rpc-message->bytevector 'message))
  (check-raises assertion-violation? (json-rpc-response-result 'response)))

(test-exit)
