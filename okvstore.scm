(define-library (ovkstore)

  (export make close begin! commit! rollback! ref set! rm! range)
  (export pack unpack *null*)

  (import (scheme base))
  (import (scheme comparator))
  (import (scheme generator))
  (import (scheme mapping))

  (import (okvstore lexicographic))

  (begin

    (define-record-type <memorydb>
      (make-memorydb store)
      memorydb?
      (store memorydb-store memorydb-store!))

    (define (make . args)
      (make-memorydb (mapping (make-default-comparator))))

    (define (close . args)
      (void))

    (define-record-type <transaction>
      (make-transaction database store)
      transaction?
      (database transaction-database transaction-database!)
      (store transaction-store transaction-store!))

    (define (begin! database . args)
      (make-transaction database (memorydb-store database)))

    (define (commit! transaction . args)
      (memorydb-store! (transaction-database transaction) (transaction-store transaction)))

    (define (rollback! transaction . args)
      (void))

    (define (ref transaction key)
      (mapping-ref/default (transaction-store transaction) key #f))

    (define (set! transaction key value)
      (transaction-store! transaction (mapping-set (transaction-store transaction) key value)))

    (define (rm! transaction key)
      (transaction-store! transaction (mapping-delete (transaction-store transaction) key)))

    (define (prefix? bytevector other)
      ;; Return #t if BYTEVECTOR is prefix of OTHER
      (if (> (bytevector-length bytevector) (bytevector-length other))
          #f
          (let loop ((index 0))
            (if (= index (bytevector-length bytevector))
                #t
                (if (not (= (bytevector-u8-ref bytevector index)
                            (bytevector-u8-ref other index)))
                    #f
                    (loop (+ index 1)))))))

    (define (range transaction prefix)
      ;; XXX: This could be better
      (let* ((store (transaction-store transaction)))
        (let loop ((key (mapping-key-successor store prefix #f))
                   (out '()))
          (if (not key)
              (list->generator (reverse out))
              (if (prefix? prefix key)
                  (loop (mapping-key-successor store key #f)
                        (cons (cons key (mapping-ref/default store key #f))))
                  (loop #f out))))))



    ))
