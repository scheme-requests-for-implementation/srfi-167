;; Copyright © 2019 Amirouche BOUBEKKI <amirouche at hyper dev>
;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
(define-library (okvs)

  (export okvs
          okvs?
          okvs-close
          okvs-transaction-begin
          okvs-transaction-metadata
          okvs-transaction?
          okvs-transaction-commit
          okvs-transaction-roll-back
          okvs-in-transaction
          okvs-ref
          okvs-set!
          okvs-delete!
          okvs-range-remove!
          okvs-range
          okvs-prefix)

  (import (only (chezscheme) void))
  (import (scheme base))
  (import (scheme case-lambda))
  (import (scheme list))
  (import (scheme hash-table))
  (import (only (scheme bytevector)
                bytevector=?
                u8-list->bytevector
                bytevector->u8-list))
  (import (scheme comparator))
  (import (scheme generator))
  (import (scheme mapping))

  (begin

    (define-record-type <memorydb>
      (make-memorydb store)
      okvs?
      (store memorydb-store memorydb-store!))

    (define (lexicographic<? bytevector other)
      (negative? (lexicographic-compare bytevector other)))

    (define vector-hash
      (comparator-hash-function
       (make-vector-comparator (make-default-comparator)
                               bytevector?
                               bytevector-length
                               bytevector-u8-ref)))

    (define (make-lexicographic-comparator)
      (make-comparator bytevector? bytevector=? lexicographic<? vector-hash))

    (define (okvs home . args)
      (make-memorydb (mapping (make-lexicographic-comparator))))

    (define (okvs-close . args)
      (void))

    (define (okvs-debug okvs proc)
      (mapping-for-each (memorydb-store okvs) proc))

    (define-record-type <transaction>
      (make-transaction database store metadata)
      okvs-transaction?
      (database transaction-database transaction-database!)
      (store transaction-store transaction-store!)
      (metadata okvs-transaction-metadata))

    (define (okvs-transaction-begin database . args)
      (make-transaction database
                        (memorydb-store database)
                        (make-hash-table (make-default-comparator))))

    (define (okvs-transaction-commit transaction . args)
      (memorydb-store! (transaction-database transaction) (transaction-store transaction)))

    (define (okvs-transaction-roll-back transaction . args)
      (void))

    (define (%okvs-in-transaction okvs proc failure success)
      (let ((transaction (okvs-transaction-begin okvs)))
        (guard (ex
                (else
                 (okvs-transaction-roll-back transaction)
                 (failure ex)))
          (call-with-values (lambda () (proc transaction))
              (lambda out
                (okvs-transaction-commit transaction)
                (apply success out))))))

    (define okvs-in-transaction
      (case-lambda
        ((okvs proc) (okvs-in-transaction okvs proc raise values))
        ((okvs proc failure) (okvs-in-transaction okvs proc failure values))
        ((okvs proc failure success) (%okvs-in-transaction okvs proc failure success))))

    (define (okvs-ref transaction key)
      (mapping-ref/default (transaction-store transaction) key #f))

    (define (okvs-set! transaction key value)
      (transaction-store! transaction (mapping-set (transaction-store transaction) key value)))

    (define (okvs-delete! transaction key)
      (transaction-store! transaction (mapping-delete (transaction-store transaction) key)))

    (define (okvs-range-remove! transaction start-key start-include? end-key end-include?)
      (let ((generator (okvs-range transaction start-key start-include? end-key end-include?)))
        (let loop ((pair (generator)))
          (unless (eof-object? pair)
            (let ((key (car pair)))
              (okvs-delete! transaction key)
              (loop (generator)))))))

    (define (lexicographic-compare bytevector other)
      ;; Return -1 if BYTEVECTOR is before OTHER, 0 if equal
      ;; and otherwise 1
      (let ((end (min (bytevector-length bytevector)
                      (bytevector-length other))))
        (let loop ((index 0))
          (if (zero? (- end index))
              (if (= (bytevector-length bytevector)
                     (bytevector-length other))
                  0
                  (if (< (bytevector-length bytevector)
                         (bytevector-length other))
                      -1
                      1))
              (let ((delta (- (bytevector-u8-ref bytevector index)
                              (bytevector-u8-ref other index))))
                (if (zero? delta)
                    (loop (+ 1 index))
                    (if (negative? delta)
                        -1
                        1)))))))

    (define (okvs-range-init store key)
      (let ((value (mapping-ref/default store key #f)))
        (if value
            (list (cons key value))
            '())))

    (define (okvs-range transaction start-key start-include? end-key end-include? . args)
      (let* ((store (transaction-store transaction)))
        (let loop ((key (mapping-key-successor store start-key (const #f)))
                   (out (if start-include?
                            (okvs-range-init store start-key)
                            '())))
          (if (not key)
              (list->generator (reverse! out))
              (case (lexicographic-compare key end-key)
                ((-1)
                 (loop (mapping-key-successor store key (const #f))
                       (cons (cons key (mapping-ref/default store key #f)) out)))
                ((0)
                 (if end-include?
                     (loop #f (cons (cons key (mapping-ref/default store key #f)) out))
                     (loop #f out)))
                ((1) (loop #f out)))))))

    (define (strinc bytevector)
      "Return the first bytevector that is not prefix of BYTEVECTOR"
      ;; See https://git.io/fj34F, TODO: OPTIMIZE
      (let ((bytes (reverse! (bytevector->u8-list bytevector))))
        ;; strip #xFF
        (let loop ((out bytes))
          (when (null? out)
            (error 'okvs "Key must contain at least one byte not equal to #xFF." bytevector))
          (if (= (car out) #xFF)
              (loop (cdr out))
              (set! bytes out)))
        ;; increment first byte, reverse and return the bytevector
        (u8-list->bytevector (reverse! (cons (+ 1 (car bytes)) (cdr bytes))))))

    (define (okvs-prefix transaction prefix . config)
      (apply okvs-range transaction prefix #t (strinc prefix) #f config))


    ))
