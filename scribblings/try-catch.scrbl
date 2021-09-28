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


@section{Introduction}

@racketmodname[try-catch] provides a combination of:

@itemlist[
 @item{The error handling from @racket[with-handlers], except with better end weight and less boilerplate}
 @item{The protection of @racket[dynamic-wind]}
 @item{The option to share code between the clauses of the @racket[dynamic-wind]}
 @item{An extra happy-path cleanup phase after the @racket[dynamic-wind] ends}
 ]

An additional macro, @racket[defatalize], is provided.  It traps all raised values and returns them.

@section{Synopsis}

@examples[
          #:eval eval
          #:label #f

	  (require try-catch)

	  (define err (defatalize (raise-arguments-error 'foo "failed")))
	  err
	  
	    (try [(displayln "ok")])
	    
	    (try [(displayln "body")
	    	  (raise 'boom)]
	    	 [catch (string? (printf "caught a string\n"))
		        (symbol? (printf "caught a symbol\n"))])

	    (try [(displayln "body")
	    	  (raise 'boom)]
	    	 [catch (string? (printf "caught a string: ~v\n" e))
		  	(symbol? (printf "'e' (the value of the exception) is: ~v\n" e))])

	    (try [pre (displayln "pre")]
 	    	 [(displayln "body")]
		 [post (displayln "post")])

	    (define down
	      (let/cc up
	        (try [pre (displayln "pre")]
 	             [(displayln "first body")
	   	      (let/cc down
	   	        (up down))
		      (displayln "second body")
		      void]
	   	     [post (displayln "post")])))
            (down (void))

	    (define down+cleanup
	      (let/cc up
	        (try [pre (displayln "pre")]
 	             [(displayln "first body")
	   	      (let/cc down
	   	        (up down))
		      (displayln "second body")
		      (raise 'boom)
		      void]
		     [post (displayln "post")]
		     [catch (symbol? (and (printf "caught a symbol: ~a" e) 'symbol-caught))]
		     [cleanup (displayln "cleanup")])))
	    down+cleanup
            (down+cleanup (void))
	    down+cleanup


	    (try [shared (define username "bob")]
	    	 [pre (printf "in pre, prepping to handle ~a.\n" username)]
 	    	 [(printf "in body. hello, ~a.\n" username)]
	    	 [post (printf "in post, goodbye ~a.\n" username)]
		 [cleanup (printf "in cleanup, done with ~a." username)])

          #reader scribble/comment-reader
	  ; Set up some mock functions for use in the following example
          (define (get-db-handle)
            (displayln "opening a database handle")
            'mock-db-handle)

	  (define (close-dbh db) (displayln "closing a database handle") #f)
	  
	  (define (is-quitting-time?) #t)
	  
          (define (query-user-data name db)
            (printf "running a db query...\n")
	    (hash "user-id"  (random 10)
	    	  "username" name
	    	  "ran-at"   (current-inexact-milliseconds)))

          #reader scribble/comment-reader
	  ;  Ensure that database handles are opened/closed as needed
	  (try [shared (define db #f)]
               [pre (set! db (get-db-handle))]
	       [(define user (query-user-data "bob" db))
	        (display "user is: ") (pretty-print user)
	        (when (is-quitting-time?) (raise "interrupted before second query because it's quitting time"))
		(query-user-data "fred" db)]
	       [post (set! db (close-dbh db))
	       	     (printf "done. final db handle value: ~a\n" db)]
     	       [catch (void (printf "ignoring exception: ~a" e))])
]


@section{Details}

@defform[(defatalize body-expr ...+)]{Evaluates the specified code.  Exceptions and other @racket[raise]d values will be trapped and become the return value of the @racketid[defatalize] expression.}

@defform[#:literals (shared pre post catch cleanup)     
         (try maybe-shared maybe-pre body maybe-post maybe-catch maybe-cleanup)
	 #:grammar	 
         [(maybe-shared (code:line) [shared expr ...+])
	 (maybe-pre   (code:line) [pre expr ...+])
	 (body [expr ...+])
	 (maybe-post (code:line) [post expr ...+])
	 (maybe-catch (code:line) [catch (predicate handler-expr) ...+])
	 (maybe-cleanup (code:line) [cleanup expr ...+])
	 (predicate (code:line (and/c procedure? (procedure-arity-includes/c 1))))
	 ]]{
Applies its clauses in order.  The final value is the value returned by the @racketid[body] (if there are no errors), or of whichever @racketid[catch] clause is executed (if there are).

The code in the @racketid[pre] clause is invoked before @racketid[body] and the code in the @racketid[post] clause is invoked after @racketid[body], regardless of how control enters/exits the @racketid[body] clause (e.g. due to a prompt abort or a continuation invocation).  No special handling is performed for jumps into or out of the @racketid[pre] and @racketid[post] clauses.

Code in the @racketid[shared] clause is visible in all subsequent clauses.  It is run only once, when the @racketid[try] expression is first entered.

Code in the @racketid[cleanup] clause is run only once, when the @racketid[body] clause completes normally.  It is not run if an error is raised.

The error handling in the @racketid[catch] clause covers all the other clauses.  It consists of a series of subclauses each of which consists of a predicate and a handler expression.  (cf @racket[with-handlers])  The handler expression will be wrapped in a one-argument procedure and the procedure will be called with the value of the exception being tested.  The argument to this function is named @racketid[e] (short for `exception') and is available in the handler.

For purposes of the @racketid[catch] clause, this: 

@racket[(try ['ok][catch (string? (displayln e))])]

Is equvalent to this:

@racket[(with-handlers ([string? (lambda (e) (displayln e))]) 'ok)]

If no @racketid[catch] clause is provided then all exceptions will be re-raised.
}
