#lang scribble/manual

@(require (for-label racket)
          racket/sandbox
          scribble/example)

@title{try-catch}

@author{David K. Storrs}

@defmodule[try-catch]


@(define eval
   (call-with-trusted-sandbox-configuration
    (lambda ()
      (parameterize ([sandbox-output 'string]
                     [sandbox-error-output 'string]
                     [sandbox-memory-limit 50])
        (make-evaluator 'racket)))))

@section{Mock Functions - Ignore This}

The following are mock functions used to make the example code look more realistic.  Skip to @secref["Introduction"].

@examples[
          #:eval eval
          #:label #f

          (define (get-db-handle)
            (displayln "\tmocking the creation of a database handle")
            'mock-db-handle)
          (define (close-dbh db) (displayln "\tfake database connection closed"))
          (define (query-rows db sql)
            (displayln "\tpretending to run a db query")
            '(a b c))
          (define (get-net-handle)
            (displayln "\tmocking the creation of a TCP connection")
            'mock-net-handle)
          (define (close-net-handle net) (displayln "\tfake TCP connection closed"))
          (define (notify who msg) (printf "\tpretending to notify the ~a! msg is: ~a\n" who msg))
          (define (ping-server net) (displayln "\tpretending to ping the server"))
          (define (send-message net msg)
            (printf "\tserializing struct and pretending to send it to the server: ~v\n" msg)
            (raise (exn:fail:network "could not connect" (current-continuation-marks))))
          (struct message (data) #:prefab)

          ]

@section{Introduction}

@racketmodname[try-catch] offers a combination of the error handling found in @racket[with-handlers] and the evaluation assurances provided by @racket[dynamic-wind], along with better end weight for error handling.

@section{Synopsis}

@examples[
          #:eval eval
          #:label #f

          #reader scribble/comment-reader
          (let ()
            ; Here is some code that you might find in an actual Racket program.
            ; It's hard to follow because the error handling obscures the actual
            ; purpose, and because the infrastructure code gets in the way.  See
            ; below for the try-catch equivalent
            (displayln "shared code/variables...")
            (define db (get-db-handle))
            (define net (get-net-handle))
            (begin0
                (dynamic-wind
                  (thunk (displayln "pre...")
                         (printf "\thandles are available: ~v, ~v\n" db net))
                  (thunk
                   (with-handlers ([(and/c exn:fail:contract?
                                           (compose1 (curry regexp-match #rx"database handle closed")
                                                     exn-message))
                                    (λ (e)
                                      (printf "catch: database handle accidentally closed")
                                      (notify 'db-admin "database might be down"))]
                                   [exn:fail:network?
                                    (λ (e)
                                      (printf "catch: failure on the example network\n")
                                      (notify 'net-admin "fake server inaccessible"))])
                     (displayln "body...")

                     (if (ping-server net)
                         (send-message net (message (query-rows db "select username from users")))
                         (raise (exn:fail:network "fake server unavailable" (current-continuation-marks))))))
                  (thunk (displayln "post...")
                         (close-dbh db)
                         (close-net-handle net)))
              (displayln "cleanup...")
              (displayln "\tdone.")))

          (require try-catch)
          (displayln "The try-catch version...")

          (try [shared (displayln "shared...")
                 (define db (get-db-handle))
                 (define net (get-net-handle))]
               [(displayln "body...")
                (if (ping-server net)
                    (send-message net (message (query-rows db "select username from users")))
                    (raise (exn:fail:network "fake server unavailable" (current-continuation-marks))))]
               [post (displayln "post...")
                     (close-dbh db)
                     (close-net-handle net)]
               [cleanup (displayln "cleanup...")
                        (displayln "\tdone.")]
               [catch
                   ([(and/c exn:fail:contract?
                            (compose1 (curry regexp-match #rx"database handle closed")
                                      exn-message))
                     (λ (e)
                       (printf "catch: database handle accidentally closed")
                       (notify 'db-admin "database might be down"))]
                    [exn:fail:network?
                     (λ (e)
                       (printf "catch: failure on the example network\n")
                       (notify 'net-admin "fake server inaccessible"))])]
               )
          ]
