(import (scheme base)
        (bark main))

(define (factorial k)
  (let lp [[k k]
           [a 1]]
    (if (zero? k) a (lp (- k 1) (* a k)))))

(test simple-procedure-passes
      (is (= (factorial 0) 1)
          (= (factorial 3) 6)
          (= (factorial 9) (* 9 8 7 6 5 4 3 2 1))))

(test simple-procedure-fails
      (test should-fail
            (is (= (factorial 4) 16)))
      (define failure #f)
      (call/cc
       (lambda (ret)
        (with-exception-handler
            (lambda (ex)
              (set! failure ex)
              (ret #t))
          should-fail)))
      (is failure))

(define (allocate-large-sytem)
  '(large-system))

(define (initialize-system! obj)
  (set-car! obj 'initialized-system))

(define (system-initialized? obj)
  (symbol=? (car obj) 'initialized-system))

(define (use-system system)
  (when (system-initialized? system)
    'result))

(define (valid-result? res)
  (symbol=? res 'result))

(define (free-system! sys)
  #f)

(test test-large-system
      (define system (allocate-large-sytem))
      (test initialization
            (is system)
            (initialize-system! system)
            (is (system-initialized? system)))
      (test usage
            (is (valid-result? (use-system system))))
      (test de-initialization
            (free-system! system))
      (run-tests!))
