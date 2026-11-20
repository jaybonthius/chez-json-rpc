# chez-json-rpc

JSON-RPC 2.0 messages and `Content-Length` framing, as language servers and debug adapters speak them, for threaded Chez Scheme 10.4.1. Two R6RS libraries: `(json-rpc framing)` reads and writes frames from pieces of bytes or from a port, and `(json-rpc)` reads, checks and writes JSON-RPC 2.0 messages with [chez-json](https://github.com/jaybonthius/chez-json)'s `(json)`. Neither starts a thread: matching answers to requests, timeouts and cancelling belong to the program that uses them.

```scheme
(import (chezscheme) (json-rpc framing) (json-rpc))

(define reader
  (make-frame-reader (lambda (stray) (display (utf8->string stray) (console-error-port)))))

(define frame
  (body->frame
    (json-rpc-message->bytevector
      (make-json-rpc-request 1 "initialize" '(("processId" . null))))))
;; "Content-Length: 74\r\n\r\n{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"processId\":null}}"

(frame-reader-read! reader (string->utf8 "Starting server\n"))
;; (), and "Starting server\n" goes to the reader's skipped procedure

(define m (bytevector->json-rpc-message (car (frame-reader-read! reader frame))))
(list (json-rpc-request-id m) (json-rpc-request-method m) (json-rpc-request-params m))
;; (1 "initialize" (("processId" . null)))

(json-rpc-message->bytevector (make-json-rpc-response 1 '(("capabilities"))))
;; the bytes of {"jsonrpc":"2.0","id":1,"result":{"capabilities":{}}}

(guard (c [(json-rpc-error? c) (list (json-rpc-error-code c) (condition-message c))])
  (bytevector->json-rpc-message (string->utf8 "[1]")))
;; (-32600 "a batch is not read")
```

## Building

The build needs threaded Chez Scheme 10.4.1 and chez-json, built with its own Makefile. `JSON_DIR` names its checkout, by default `../chez-json`.

```sh
make SCHEME=/path/to/chez-10.4.1/scheme JSON_DIR=/path/to/chez-json deps
make SCHEME=/path/to/chez-10.4.1/scheme JSON_DIR=/path/to/chez-json build
make SCHEME=/path/to/chez-10.4.1/scheme JSON_DIR=/path/to/chez-json test
```

`make deps` builds chez-json. `CAP` prefixes every build and test process, for example `CAP="systemd-run --user --scope -q -p MemoryMax=8G"`.

Programs import the libraries with `--libdirs /path/to/chez-json-rpc/build/obj/src:/path/to/chez-json/build/obj/src`. A program that uses only `(json-rpc framing)` needs no chez-json.

## Framing: `(json-rpc framing)`

A frame is a header block, a `Content-Length:` line, any other header lines and an empty line, then a body of that many bytes. Header names are matched whatever their case, and a header line may end in CRLF or LF. Bytes before a `Content-Length:` header, wherever it starts, are **stray**: they go to the reader's `skipped` procedure as they arrive, and are never held. So a server that prints to its output, with or without a line end, or one that miscounts a body's length, loses only the frames that output overlaps, and the reader finds the next header.

A **frame error** is a header line after the `Content-Length:` line longer than 8,192 bytes, a `Content-Length` that is not a byte count, a second `Content-Length` in one header block, or a body longer than `frame-max-length`. A peer that sends a header without end, or a huge length, must not make the reader buffer or allocate without bound. The bad frame's bytes are dropped as they arrive, and reading goes on; when the length is unknown, its body is read as stray bytes.

| Form | What it does |
|---|---|
| `(make-frame-reader [skipped])` | a reader of frames from pieces of bytes; `skipped`, a procedure of a bytevector, takes the stray bytes, and by default drops them. The reader takes `frame-max-length` as it is when it is made. A reader holds state and belongs to one stream and one thread |
| `(frame-reader-read! reader bytes)` | the list of what `bytes` completes, oldest first: each frame's body, a fresh bytevector, and in the place of each frame error a `&frame-error` condition. It never raises for a frame error, keeps an incomplete frame for the next call and never changes `bytes`. Stray bytes go to `skipped` during the call, so they reach it before the call's bodies are returned |
| `(read-frame port [skipped])` | one body from a binary input port, blocking, by the same rules, or the end-of-file object when the input ends between frames. It reads no further than its frame. At a frame error it consumes the bad frame, then raises `&frame-error`, so the next call reads on; it raises `&frame-error` when the input ends inside a frame |
| `(body->frame body)` | the header and `body` as one bytevector |
| `frame-max-length` | thread parameter, default 256 MiB (268435456): the longest body read |
| `frame-reader?`, `frame-error?`, `&frame-error` | the predicates and the condition type, a child of `&error` with a who and a message |

## Messages: `(json-rpc)`

Messages are immutable records whose fields are JSON values as `(json)` reads them: objects as association lists with string keys, arrays as vectors, `null` as the symbol `null`. An id is an exact integer or a string; a response's id may also be `null`, for a request whose id could not be read. Params are an object, an array, or `null` for none.

| Form | What it does |
|---|---|
| `(make-json-rpc-request id method [params])`, `json-rpc-request?`, `json-rpc-request-id`, `json-rpc-request-method`, `json-rpc-request-params` | a request; `params` defaults to `null` |
| `(make-json-rpc-notification method [params])`, `json-rpc-notification?`, `json-rpc-notification-method`, `json-rpc-notification-params` | a notification, with no id |
| `(make-json-rpc-response id result)`, `(make-json-rpc-error-response id condition)`, `json-rpc-response?`, `json-rpc-response-id` | a response, holding a result or a `&json-rpc-error` condition |
| `(json-rpc-response-result response)` | the result; raises for an error response, since `#f` and `null` are results a peer may send |
| `(json-rpc-response-error response)` | the error response's condition, for the caller to raise unchanged, or `#f` |
| `(json->json-rpc-message value)` | the message a JSON value already read is |
| `(bytevector->json-rpc-message body)` | the message a body holds; raises `&json-rpc-error` with `json-rpc-parse-error-code` for a body that is not JSON |
| `(json-rpc-message->bytevector message)` | the message as UTF-8 JSON; `null` params and `null` error data are left out |
| `(make-json-rpc-error code message [data])` | a `&json-rpc-error` condition with a message; `data` defaults to `null` |
| `json-rpc-error?`, `json-rpc-error-code`, `json-rpc-error-data`, `&json-rpc-error` | the condition type, a child of `&error`; its message is read with `condition-message` |
| `json-rpc-parse-error-code`, `json-rpc-invalid-request-code`, `json-rpc-method-not-found-code`, `json-rpc-invalid-params-code`, `json-rpc-internal-error-code` | -32700, -32600, -32601, -32602, -32603 |

Both readers raise `&json-rpc-error` with `json-rpc-invalid-request-code`, and a message naming the fault, for a value that is not one message: a batch, a `jsonrpc` member other than `"2.0"`, a method that is not a string, a request with a result or an error, a request id that is not an exact integer or a string, params that are not an object, an array or `null`, a response with both a result and an error or with no id, or an error object without an integer code and a string message. Other members are ignored.
