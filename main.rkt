#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/function)

(provide try defatalize)

(define-syntax (try stx)

  ; The datum->syntax syntax->datum dance is to non-hygienically make 'e' available
  (define-syntax-class catch-clause
    (pattern (p:expr a:expr)
             #:with pred #'(λ (e) (p e))
             #:with action (datum->syntax stx (syntax->datum #'(λ (e) a)))))

  (syntax-parse stx
    #:datum-literals (shared try pre catch post cleanup)
    [(try
      (~optional [shared   shared-expr:expr    ...])
      (~optional [pre      pre-expr:expr       ...])
      [(~peek-not (~or shared pre)) body:expr ...+]
      (~optional [post     post-expr:expr      ...])
      (~optional [catch    clause:catch-clause ...])
      (~optional [cleanup  cleanup-expr:expr   ...])
      )
     #'(with-handlers ((~? (~@ [clause.pred clause.action] ...)
                           [(λ (e) #t) raise])) ; by default, re-raise the exception
         (~? (~@ shared-expr ...))
         (begin0
             ;; (thunk) is an error, so the '(void)' expressions
             ;; ensure syntactic validity if their clause wasn't used.
             (dynamic-wind
               (~? (thunk (~@ pre-expr ...))  void)
               (thunk body ...)
               (~? (thunk (~@ post-expr ...)) void))
           (~? (~@ cleanup-expr ...))))]))


(define-syntax (defatalize stx)
  (syntax-parse stx
    [(defatalize body ...+)
     #'(with-handlers ([exn:break? raise]
                       [(λ (e) #t) identity])
         body ...)]))


(module+ test
  (require racket/port rackunit)

  (define-check (check-output-string actual-thunk expected-string)
    (check-equal? (with-output-to-string actual-thunk) expected-string))


  ;   Verify that check-output-string does as expected with base Racket code
  (check-output-string (thunk (displayln "ok")) "ok\n")

  (check-output-string (thunk
                        (dynamic-wind
                          (thunk (display "pre "))
                          (thunk (display "body "))
                          (thunk (display "post"))))
                       "pre body post")

  (check-output-string (thunk
                        (define down
                          (let/cc up
                            (dynamic-wind
                              (thunk (display "pre "))
                              (thunk (display "body ")
                                     (let/cc down
                                       (up down))
                                     (display "after-let/cc ")
                                     void)
                              (thunk (display "post ")))))
                        (display "after-down ")
                        (down (void)))
                       "pre body post after-down pre after-let/cc post after-down ")


  ; Verify the `try` and `defatalize` macros
  (check-true (exn? (defatalize (raise-arguments-error 'foo "bar" 7)))
              "defatalize returns an exn")
  (check-true (integer? (defatalize (raise 7)))
              "defatalize returns a non-exn")

  (check-output-string (thunk (try [(display "body ")
                                    (raise "oops")]
                                   [catch
                                       (string? (display e))]))
                       "body oops")

  (check-output-string (thunk (try [(display "body ")
                                    (raise 7)]
                                   [catch
                                       (string? (display e))
                                     (void (display (format "non-string: ~a" e)))]))
                       "body non-string: 7")

  (check-output-string (thunk (try [(display "ok")])) "ok")

  (check-output-string (thunk
                        (with-handlers ([void (λ (e) (display "no 'catch' clause"))])
                          (try [(display "body of try. ") (raise #f)])))
                       "body of try. no 'catch' clause")

  (check-output-string (thunk
                        (try [(display "body, ") (raise "error")]
                             [catch (string? (display (format "oops:~a" e)))]))
                       "body, oops:error")

  (check-output-string (thunk
                        (define down
                          (let/cc up
                            (try [shared (define db 'not-ready)]
                                 [pre
                                  (display (format "in pre before setting, db is: ~v. " db))
                                  (set! db 'ready)]
                                 [(display (format "in body, db is: ~v. " db))
                                  (let/cc down (up down))
                                  (display "after-let/cc. ")
                                  void]
                                 [post
                                  (display (format "entering post, db is: ~v. " db))
                                  (set! db 'not-ready)
                                  (display (format "leaving post, db is: ~v. " db))]
                                 [catch (void (display (format "shouldn't get here:~a" e)))]
                                 [cleanup
                                  (set! db 'finalized)
                                  (display (format "in cleanup, db is: ~v. " db))])))
                        (display "after defining down. ")
                        (down (void)))
                       "in pre before setting, db is: 'not-ready. in body, db is: 'ready. entering post, db is: 'ready. leaving post, db is: 'not-ready. after defining down. in pre before setting, db is: 'not-ready. after-let/cc. entering post, db is: 'ready. leaving post, db is: 'not-ready. in cleanup, db is: 'finalized. after defining down. ")

  (check-output-string
   (thunk (try [(raise "foo")]
               [catch
                   [(λ (e) (and (string? e) (odd? (string-length e))))
                    (display "odd")]
                   [(λ (e) (and (string? e) (even? (string-length e))))
                    (display "even")]]))
   "odd")
  
  (check-output-string
   (thunk (try [(raise "foobar")]
               [catch
                   [(λ (e) (and (string? e) (odd? (string-length e))))
                    (display "odd")]
                   [(λ (e) (and (string? e) (even? (string-length e))))
                    (display "even")]]))
   "even")
  
  )
