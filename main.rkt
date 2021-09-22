#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         racket/function)

(provide  (all-defined-out))

(define-syntax (try stx)
  (define-syntax-class catch-clause
    (pattern [pred:expr action:expr]))

  (syntax-parse stx
    #:datum-literals (shared try pre catch post cleanup)
    [(try
      (~between [shared shared-expr:expr ...]         0 1 #:name #''shared)
      (~between [pre pre-expr:expr ...]               0 1 #:name #''pre)
      (~between [body:expr ...+]                      1 1 #:name #''body)
      (~between [catch (clause:catch-clause ...+)]    0 1 #:name #''catch)
      (~between [post post-expr:expr ...]             0 1 #:name #''post)
      (~between [cleanup cleanup-expr:expr ...]       0 1 #:name #''cleanup )
      )
     #'(with-handlers (clause ...)
         (~? (~@ shared-expr ...))
         (begin0
             ;; (thunk) is an error, so the '(void)' expressions
             ;; ensure syntactic validity if their clause wasn't used.
             (dynamic-wind
               (thunk (void) pre-expr ...)
               (thunk body ...)
               (thunk (void) post ...))
           cleanup-expr ...))]))


(define-syntax (defatalize stx)
  (syntax-parse stx
    [(defatalize body ...+)
     #'(with-handlers ([exn:break? raise]
                       [any/c identity])
         body ...)]))
