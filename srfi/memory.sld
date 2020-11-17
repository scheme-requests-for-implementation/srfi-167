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
(define-library (srfi 167 memory)

  (export okvs-open
          okvs?
          okvs-close
          make-default-state
          okvs-transaction?
          okvs-transaction-state
          okvs-in-transaction
          okvs-ref
          okvs-set!
          okvs-delete!
          okvs-range-remove!
          okvs-range
          okvs-prefix-range
          okvs-hook-on-transaction-begin
          okvs-hook-on-transaction-commit
          make-default-engine)

  (import (scheme base))
  (import (scheme case-lambda))
  (import (srfi 1))
  (import (srfi 125))
  (import (srfi 128))
  (import (srfi 145))
  (import (srfi 146))
  (import (srfi 158))
  (import (srfi 173))
  (import (srfi 167 pack))
  (import (srfi 167 engine))

  (include "memory.scm"))
