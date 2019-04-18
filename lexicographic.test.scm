(define-library (okvstore lexicographic.test)
  (export test-1)
  (export test-2)

  (import (scheme base))
  (import (azul tests))
  (import (okvstore lexicographic))
  (import (only (chezscheme) bitwise-arithmetic-shift-left))

  (begin

    (define test-1
      (test (list -132 -3000)
            (unpack (pack -132 -3000))))

    (define test-2
      (test (list (bitwise-arithmetic-shift-left 1 (* 8 10))
                  (- (bitwise-arithmetic-shift-left 1 (* 8 10)))
                  'echo
                  -1
                  255
                  -255
                  "hello"
                  #vu8(101 42)
                  0
                  #t
                  #f
                  *null*)
            (unpack (pack (bitwise-arithmetic-shift-left 1 (* 8 10))
                          (- (bitwise-arithmetic-shift-left 1 (* 8 10)))
                          'echo
                          -1
                          255
                          -255
                          "hello"
                          #vu8(101 42)
                          0
                          #t
                          #f
                          *null*))))

    ))
