#lang racket/base

(require net/url
          net/uri-codec)

(define (post-request post-url post-data)
  (define-values
    (response headers input-port)
    (http-sendrecv/url post-url
                       #:method "POST"
                       #:data
                       post-data
                       #:headers
                       (list
                        "Content-Type: application/x-www-form-urlencoded")))
  (define response-data (read-json input-port))
  (close-input-port input-port)
  response-data)
