(define-library (srfi 167 pack test)

  (export run-tests)

  (import (scheme base))
  (import (chibi test))
  (import (srfi 167 pack))

  (begin

    (define expected
      (list *null*
            #t
            #f
            0
            #u8(42 101 255)
            "hello world"
            'symbol
            42
            (expt 2 64)
            -42
            (- (expt 2 64))
            (list)
            (list *null*)
            (list #t #f)
            (list 0 42 -42)
            (list (list 0 42) -42)
            (list (expt 2 64) (- (expt 2 64)))
            (list #u8(42 101 255))
            (list "hello world")
            (list 'symbol)))

    (define (run-tests)
      (for-each (lambda (value) (test value (unpack (pack value)))) expected))))
