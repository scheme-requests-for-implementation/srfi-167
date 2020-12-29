(define *null* '(null))

;; reserved: #x03, #x04. #x23, #x24
(define *null-code* #x00)
;; variable length
(define *bytes-code* #x01)
(define *string-code* #x02)
(define *nested-code* #x05)
(define *symbol-code* #x06)
;; integers
(define *neg-int-start* #x0B)
(define *int-zero-code* #x14)
(define *pos-int-end* #x1D)
;; double
(define *double-code* #x21)
;; true and false
(define *false-code* #x26)
(define *true-code* #x27)
(define *escape-code* #xFF)

;; pack

(define (struct:pack>Q integer)
  (let ((bytevector (make-bytevector 8 0)))
    (let loop ((index 0))
      (unless (= index 8)
        (bytevector-u8-set! bytevector
                            index (bitwise-and
                                   (arithmetic-shift integer (- (* (- 7 index) 8)))
                                   #xFF))
        (loop (+ index 1))))
    bytevector))

(define (struct:unpack>Q bytevector)
  (let loop ((index 0)
             (out 0))
    (if (= index 8)
        out
        (loop (+ index 1)
              (+ out
                 (arithmetic-shift
                  (bytevector-u8-ref bytevector index)
                  (* (- 7 index) 8)))))))

(define (%%pack-bytes bv accumulator)
  (let loop ((index 0))
    (unless (= index (bytevector-length bv))
      (let ((byte (bytevector-u8-ref bv index)))
        (if (zero? byte)
            (begin ;; escape null byte
              (accumulator #x00)
              (accumulator *escape-code*))
            (accumulator byte))
        (loop (+ index 1)))))
  (accumulator #x00))

(define *bigish* (arithmetic-shift 1 (* 8 8)))

(define *limits*
  (let ((limits (make-vector 9)))
    (let loop ((index 0))
      (unless (= index 9)
        (vector-set! limits index (- (arithmetic-shift 1 (* index 8)) 1))
        (loop (+ index 1))))
    limits))

(define (bisect vector value)
  (let loop ((low 0)
             (high (vector-length vector)))
    (if (>= low high)
        low
        (let ((middle (quotient (+ low high) 2)))
          (if (< (vector-ref vector middle) value)
              (loop (+ middle 1) high)
              (loop low middle))))))

(define (%%pack-positive-integer integer accumulator)
  (if (< integer *bigish*)
      ;; small integer
      (let* ((length (integer-length integer))
             (n (exact (ceiling (/ length 8))))
             (bv (struct:pack>Q integer)))
        (accumulator (+ *int-zero-code* n))
        (let loop ((index (- (bytevector-length bv) n)))
          (unless (= index (bytevector-length bv))
            (accumulator (bytevector-u8-ref bv index))
            (loop (+ index 1)))))
      ;; big integer
      (let ((length (exact (floor (/ (+ (integer-length integer) 7) 8)))))
        (accumulator *pos-int-end*)
        (accumulator length)
        (let loop ((index (- length 1)))
          (unless (= index -1)
            (accumulator (bitwise-and (arithmetic-shift integer (- (* 8 index)))
                                      #xFF))
            (loop (- index 1)))))))

(define (%%pack-negative-integer integer accumulator)
  (if (< (- integer) *bigish*)
      ;; small negative integer
      (let* ((n (bisect *limits* (- integer)))
             (maxv (vector-ref *limits* n))
             (bv (struct:pack>Q (+ maxv integer))))
        (accumulator (- *int-zero-code* n))
        (let loop ((index (- (bytevector-length bv) n)))
          (unless (= index (bytevector-length bv))
            (accumulator (bytevector-u8-ref bv index))
            (loop (+ index 1)))))
      ;; big negative integer
      (let* ((length (exact (ceiling (/ (+ (integer-length integer) 7) 8))))
             (integer (+ integer (- (arithmetic-shift 1 (* length 8)) 1))))
        (accumulator *neg-int-start*)
        (accumulator (bitwise-xor length #xFF))
        (let loop ((index (- length 1)))
          (unless (= index -1)
            (accumulator (bitwise-and (arithmetic-shift integer (- (* 8 index)))
                                      #xFF))
            (loop (- index 1)))))))

(define (%%pack-list items accumulator)
  (accumulator *nested-code*)
  (for-each (%%pack accumulator #t) items)
  (accumulator #x00))

(define (%%pack accumulator . nested)
  (lambda (value)
    (cond
     ((eq? value *null*)
      (accumulator *null-code*)
      (when (pair? nested)
        (accumulator #xFF)))
     ((eq? value #t) (accumulator *true-code*))
     ((eq? value #f) (accumulator *false-code*))
     ((bytevector? value) (accumulator *bytes-code*) (%%pack-bytes value accumulator))
     ((string? value) (accumulator *string-code*) (%%pack-bytes (string->utf8 value) accumulator))
     ((symbol? value)
      (accumulator *symbol-code*)
      (%%pack-bytes (string->utf8 (symbol->string value)) accumulator))
     ;; integer
     ((and (number? value) (exact? value) (< value 0)) (%%pack-negative-integer value accumulator))
     ((and (number? value) (exact? value) (= value 0)) (accumulator *int-zero-code*))
     ((and (number? value) (exact? value) (> value 0)) (%%pack-positive-integer value accumulator))
     ;; list
     ((list? value) (%%pack-list value accumulator))
     (else (error 'pack "unsupported data type" value)))))

(define (%pack arg accumulator)
  ((%%pack accumulator) arg))

(define (pack arg)
  (let ((accumulator (bytevector-accumulator)))
    (%pack arg accumulator)
    (accumulator (eof-object))))

;; unpack

(define (list->bytevector list)
  (let ((vec (make-bytevector (length list) 0)))
    (let loop ((i 0) (list list))
      (if (null? list)
          vec
          (begin
            (bytevector-u8-set! vec i (car list))
            (loop (+ i 1) (cdr list)))))))

(define (unpack-bytes bv position)
  (let loop ((position position)
             (out '()))
    (if (zero? (bytevector-u8-ref bv position))
        (cond
         ;; end of bv
         ((= (+ position 1) (bytevector-length bv))
          (values (list->bytevector (reverse out)) (+ position 1)))
         ;; escaped null bytes
         ((= (bytevector-u8-ref bv (+ position 1)) *escape-code*)
          (loop (+ position 2) (cons #x00 out)))
         ;; end of string
         (else (values (list->bytevector (reverse out)) (+ position 1))))
        ;; just a byte
        (loop (+ position 1) (cons (bytevector-u8-ref bv position) out)))))

(define (unpack-positive-integer bv code position)
  (let* ((n (- code 20))
         (sub (make-bytevector 8 0)))
    (let loop ((index 0))
      (unless (= index n)
        (bytevector-u8-set! sub (+ (- 8 n) index) (bytevector-u8-ref bv (+ position 1 index)))
        (loop (+ index 1))))
    (values (struct:unpack>Q sub) (+ position 1 n))))

(define (unpack-negative-integer bv code position)
  (let* ((n (- 20 code))
         (maxv (vector-ref *limits* n))
         (sub (make-bytevector 8 0)))
    (let loop ((index 0))
      (unless (= index n)
        (bytevector-u8-set! sub (+ (- 8 n) index) (bytevector-u8-ref bv (+ position 1 index)))
        (loop (+ index 1))))
    (values (- (struct:unpack>Q sub) maxv) (+ position 1 n))))

(define (unpack-bigish-positive-integer bv code position)
  (let ((length (bytevector-u8-ref bv (+ position 1))))
    (values (let loop ((range (iota length))
                       (out 0))
              (if (null? range)
                  out
                  (loop (cdr range) (+ (arithmetic-shift out 8)
                                       (bytevector-u8-ref bv (+ position 2 (car range)))))))
            (+ position 2 length))))

(define (unpack-bigish-negative-integer bv code position)
  (let ((length (bitwise-xor (bytevector-u8-ref bv (+ position 1)) #xFF)))
    (values (let loop ((range (iota length))
                       (out 0))
              (if (null? range)
                  (+ (- out (arithmetic-shift 1 (* length 8))) 1)
                  (loop (cdr range) (+ (arithmetic-shift out 8)
                                       (bytevector-u8-ref bv (+ position 2 (car range)))))))
            (+ position 2 length))))

(define (unpack-list bv code position)
  (let loop ((items '())
             (position (+ position 1)))
    (if (= position (bytevector-length bv))
        (values (reverse items) (+ position 1))
        (if (zero? (bytevector-u8-ref bv position))
            (if (and (< (+ position 1) (bytevector-length bv))
                     (= (bytevector-u8-ref bv (+ position 1)) #xFF))
                (loop (cons *null* items)
                      (+ position 2))
                (values (reverse items) (+ position 1)))
            (call-with-values (lambda () (%unpack bv position))
              (lambda (value position) (loop (cons value items) position)))))))

(define (%unpack bv position)
  (let ((code (bytevector-u8-ref bv position)))
    (cond
     ;; null, true, false and zero
     ((= code *null-code*) (values *null* (+ position 1)))
     ((= code *true-code*) (values #t (+ position 1)))
     ((= code *false-code*) (values #f (+ position 1)))
     ((= code *int-zero-code*) (values 0 (+ position 1)))
     ;; variable length
     ((= code *bytes-code*) (unpack-bytes bv (+ position 1)))
     ((= code *string-code*)
      (call-with-values (lambda () (unpack-bytes bv (+ position 1)))
        (lambda (value position) (values (utf8->string value) position))))
     ((= code *symbol-code*)
      (call-with-values (lambda () (unpack-bytes bv (+ position 1)))
        (lambda (value position) (values (string->symbol (utf8->string value)) position))))
     ;; integers
     ((and (> code *int-zero-code*) (< code *pos-int-end*))
      (unpack-positive-integer bv code position))
     ((and (> code *neg-int-start*) (< code *int-zero-code*))
      (unpack-negative-integer bv code position))
     ((= code *pos-int-end*)
      (unpack-bigish-positive-integer bv code position))
     ((= code *neg-int-start*)
      (unpack-bigish-negative-integer bv code position))
     ;; list
     ((= code *nested-code*)
      (unpack-list bv code position))
     ;; oops
     (else (error 'unpack "unsupported code" code)))))

(define (unpack bv)
  (call-with-values (lambda () (%unpack bv 0))
    (lambda (value position)
      (if (= position (bytevector-length bv))
          value
          (error 'unpack "trailing garbage following value")))))
