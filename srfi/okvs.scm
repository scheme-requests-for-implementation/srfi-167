(define-library (ovks)

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

  (import (scheme base))
  (import (scheme list))
  (import (scheme hash-table))
  (import (scheme bytevector))
  (import (scheme comparator))
  (import (scheme generator))
  (import (scheme mapping))

  (begin

    (define-record-type <memorydb>
      (make-memorydb store)
      okvs?
      (store memorydb-store memorydb-store!))

    (define (lexicographic<? bytevector other)
      (let ((end (min (bytevector-length bytevector)
                      (bytevector-length other))))
        (let loop ((index 0))
          (if (negative? (- end index))
              #f
              (let ((delta (- (bytevector-u8-ref bytevector index)
                              (bytevector-u8-ref other index))))
                (if (zero? delta)
                    (loop (+ 1 index))
                    (if (negative? delta)
                        #t
                        #f)))))))

    (define (make-lexicographic-comparator)
      (make-comparator bytevector? bytevector=? lexicographic<? vector-hash))

    (define (okvs home create? . args)
      (make-memorydb (mapping (make-lexicographic-comparator))))

    (define (okvs-close . args)
      (void))

    (define (okvs-debug okvs proc)
      (mapping-for-each (memorydb-store okvs) proc))

    (define-record-type <transaction>
      (make-transaction database store)
      okvs-transaction?
      (database transaction-database transaction-database!)
      (store transaction-store transaction-store!)
      (metadata okvs-transaction-metadata))

    (define (okvs-transaction-begin! database . args)
      (make-transaction database
                        (memorydb-store database)
                        (make-hash-table (make-default-comparator))))

    (define (okvs-transaction-commit! transaction . args)
      (memorydb-store! (transaction-database transaction) (transaction-store transaction)))

    (define (okvs-transaction-rollback! transaction . args)
      (void))

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
      (let ((end (min (bytevector-length bytevector)
                      (bytevector-length other))))
        (let loop ((index 0))
          (if (negative? (- end index))
              0
              (let ((delta (- (bytevector-u8-ref bytevector index)
                              (bytevector-u8-ref other index))))
                (if (zero? delta)
                    (loop (+ 1 index))
                    (if (< delta 0)
                        -1
                        1)))))))

    (define (okvs-range-init store key)
      (let ((value (mapping-ref/default store key #f)))
        (if value
            (list (cons key value))
            '())))

    (define (okvs-range transaction start-key start-include? end-key end-include? . args)
      (let* ((store (transaction-store transaction)))
        (let loop ((key (mapping-key-successor store start-key #f))
                   (out (if start-include?
                            (okvs-range-init store start-key)
                            '())))
          (if (not key)
              (list->generator (reverse! out))
              (case (lexicographic-compare end-key key)
                ((-1)
                 (loop (mapping-key-successor store key #f)
                       (cons (cons key (mapping-ref/default store key #f)) out)))
                ((0)
                 (if end-include?
                     (loop #f
                           (cons (cons key (mapping-ref/default store key #f)) out))
                     (loop #f out)))
                ((-1) (loop #f out)))))))

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
