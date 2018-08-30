#lang racket
(require "../kara/lang/kara.rkt")

(struct register
  ([contents #:mutable #:auto])
  #:auto-value '*unassigned*)

(def stack%
  (class object%
    ; Field
    (init [s-init null])
    (def s s-init)

    (super-new)

    ; Methods
    (define/public (push x)
      (cons! x s))

    (define/public (pop)
      (let ([top (car s)])
        (set! s (cdr s))
        top))

    (define/public (init)
      (set! s null))))
