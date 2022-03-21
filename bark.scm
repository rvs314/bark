(cond-expand
 [r7rs
  (define-library (bark)
    (export *all-tests* test-fail
            is isnt
            test run-tests)
    (import (scheme base))

    (begin
     (define *test-stack* (make-parameter '()))
     (define *all-tests* (make-parameter #f))

     (define (test-fail form)
       (error
        (if (pair? (*test-stack*))
            (apply string-append
                   "Assertion Error in test stack ( "
                   (append (map (lambda (t) (string-append (symbol->string t) " "))
                                (reverse (*test-stack*)))
                           '(")")))
              
            "Assertion Failure:")
        form))

     (define-syntax is
       (syntax-rules ()
         [(is (predicate args ...))
          (let* ([rgs (list args ...)]
                 [res (apply predicate rgs)])
            (unless res
              (test-fail `(predicate ,@rgs))))]))

     (define-syntax isnt
       (syntax-rules ()
         [(is (predicate args ...))
          (let* ([rgs (list args ...)]
                 [res (apply predicate rgs)])
            (when res
              (test-fail `(predicate ,@rgs))))]))

     (define-syntax test
       (syntax-rules ()
         [(test name body body* ...)
          (begin (define (name)
                   (parameterize ([*test-stack* (cons 'name (*test-stack*))]
                                  [*all-tests* '()])
                     body body* ...
                     (run-tests)))
                 (*all-tests* (cons name (*all-tests*))))]))

     (define (run-tests)
       (for-each (lambda (k) (k)) (*all-tests*)))))])
  
