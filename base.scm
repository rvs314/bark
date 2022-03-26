(define-library (bark base)
  (export *all-tests* fail-test
          is isnt
          test run-tests!)

  (import (scheme base))

  (begin
    (define *test-stack* (make-parameter '()))
    (define *all-tests* (make-parameter '()))

    (define (fail-test message initial with-args return-value)
      (define (test-trace)
        (apply
         string-append
         " in ("
         (let lp [[rs '(")")]
                  [ts (*test-stack*)]]
           (cond [(null? ts)       (error "Test stack should never be null when printed")]
                 [(null? (cdr ts)) (cons (symbol->string (car ts)) rs)]
                 [else             (lp (cons " " (cons (symbol->string (car ts)) rs))
                                       (cdr ts))]))))
      (apply
       error
       (string-append
        message
        (if (pair? (*test-stack*)) (test-trace) "")
        ":")
       (if with-args
           (list initial '=> with-args '=> return-value)
           (list initial '=> return-value))))

    (define-syntax is
      (syntax-rules ()
        [(_ x y zs ...)
         (begin (is x) (is y) (is zs) ...)]
        [(_ (predicate args ...))
         (let* ([rgs (list args ...)]
                [res (apply predicate rgs)])
           (unless res
             (fail-test "Assertion Failure"
                        '(predicate args ...)
                        `(predicate ,@rgs)
                        res)))]
        [(_ obj)
         (unless obj
           (fail-test "Assertion Failure"
                      'obj #f obj))]))

    (define-syntax isnt
      (syntax-rules ()
        [(_ x y zs ...)
         (begin (isnt x) (isnt y) (isnt zs) ...)]
        [(_ (predicate args ...))
         (let* ([rgs (list args ...)]
                [res (apply predicate rgs)])
           (when res
             (fail-test "Denial Failure"
                        '(predicate args ...)
                        `(predicate ,@rgs)
                        res)))]
        [(_ obj)
         (when obj
           (fail-test "Denial Failure"
                      'obj #f obj))]))

    (define-syntax test
      (syntax-rules ()
        [(_ name body body* ...)
         (begin (define (name)
                  (parameterize ([*test-stack* (cons 'name (*test-stack*))]
                                 [*all-tests* '()])
                    body body* ...))
                (*all-tests* (cons name (*all-tests*))))]))

    (define (run-tests!)
      (for-each (lambda (k) (k)) (reverse (*all-tests*))))))
