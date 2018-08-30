#lang racket
(require "kara-lang/kara.rkt")

(struct Register
  (name
   [contents #:mutable #:auto])
  #:auto-value '*unassigned*)


(def stack%
  (class object%
    (super-new)

    ; Field
    (init [s-init null])
    (def s s-init)


    ; Methods
    (define/public (push x)
      (cons! x s))

    (define/public (pop)
      (let ([top (car s)])
        (set! s (cdr s))
        top))

    (define/public (init)
      (set! s null))))


(def machine
  (class object%
    (super-new)

    ; Inits
    (init [pcI           (Register 'pc)])
    (init [flagI         (Register 'flag)])
    (init [stackI        (new stack%)])
    (init [instructionsI null])

    ;Fields
    (def pc           pcI)
    (def flag         flagI)
    (def stack        stackI)
    (def instructions instructionsI)

    ; These two fields depend on the inits.
    (def the-ops
      (list (cons 'initialize-stack
                  (lam ()
                    (send stack init)))))

    (def register-table
      (list pc flag))

    ; Getters
    (define/public (get-stack)
      stack)

    (define/public (get-ops)
      the-ops)

    (define/public (start)
      (set-Register-contents! pc instructions)
      (execute))

    ; Setters
    (define/public (set-instructions seq)
      (set! instructions seq))

    ; Methods
    (define/public (lookup-register name)
      (let loop ([ls register-table])
        (match ls
          [(list)
           'NOT-FOUND]

          [(cons rfocus rrest)
           (match rfocus
             [(Register (== name) _)
              rfocus]

             [_
              (loop rest)])])))

    (define/public (allocate-register name)
      (match (lookup-register name)
        ['NOT-FOUND
         (cons! (cons name (Register name))
                register-table)]

        [_
         (error "Register already defined" name)]))

    (define/public (install-operations ops)
      (append! the-ops ops))

    (define (execute)
      (match (Register-contents pc)
        [(list)
         'DONE]

        [ins
         (let ([todo
                (Instr-proc (car ins))])
           todo)
         ; And then we do it again
         (execute)]))))
