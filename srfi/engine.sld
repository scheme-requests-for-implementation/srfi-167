(define-library (srfi 167 engine)

  (export
   make-engine
   engine?
   engine-open
   engine-close
   engine-in-transaction
   engine-ref
   engine-set!
   engine-delete!
   engine-range-remove!
   engine-range
   engine-prefix-range
   engine-hook-on-transaction-begin
   engine-hook-on-transaction-commit
   engine-pack
   engine-unpack
   engine-null)

  (import (scheme base))

  (include "engine.scm"))
