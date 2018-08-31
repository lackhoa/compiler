#lang racket
(require "kara-lang/kara.rkt")

(struct Register
  (name
   [contents #:mutable
             #:auto])
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


; Instructions
(struct Instruction
  (text
   [execution-proc #:mutable
                   #:auto])
  #:auto-value null)

(def (update-insts! insts labels machine)
  (let ([pc
         (send machine get-register 'pc)]
        [flag
         (send machine get-register 'flag)]
        [stack
         (send machine get-stack)]
        [ops
         (send machine get-ops)])
    (for-each
      (lam (inst)
        (set-Instruction-execution-proc!
          inst
          (make-execution-procedure
            (Instruction-text inst)
            labels machine pc flag
            stack ops)))
      insts)))

(def (make-execution-procedure inst labels machine
                               pc flag stack ops)
  (match (car inst)
    ['assign
     (make-assign inst machine
                  labels ops pc)]

    ['test
     (make-test inst machine labels
                ops flag pc)]

    ['branch
     (make-branch inst machine
                  labels flag pc)]

    ['goto
     (make-goto inst machine
                labels pc)]

    ['save
     (make-save inst machine
                stack pc)]

    ['restore
     (make-restore inst machine
                   stack pc)]

    ['perform
     (make-perform inst machine
                   labels ops pc)]

    [_
     (error "Unknown instruction type" inst)]))

; The machine model
(def machine
  (class object%
    (super-new)

    ;Fields
    (def pc           (Register 'pc))
    (def flag         (Register 'flag))
    (def stack        (new stack%))
    (def instructions null)

    ; These two fields depend on the others.
    (def the-ops
      (list
        (Instruction 'initialize-stack
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

        [(cons next-inst _)
         ((Instruction-execution-proc next-inst))
         ; And then we do it again
         (execute)]))))


; The assembler
(def (assemble controller-text machine)
  (let-values ([(insts labels)
                (extract-labels controller-text)])
    (lam (insts labels)
      (update-insts! insts
                     labels
                     machine)
      insts)))

(struct Label-entry
  (name insts))

(def (lookup-label labels label-name)
  (match labels
    [(list)
     (error "Undefined label" label-name)]

    ; Matches
    [(cons (Label-entry (== label-name eq?)
                        insts)
           _)
     insts]

    ; Doesn't match
    [(cons _ rest-labels)
     (lookup-label rest-labels
                   label-name)]))

(def (extract-labels text)
  (match text
    [(list)
     (values null null)]

    [(cons next-inst
           rest-insts)
     (let-values ([(insts labels)
                   (extract-labels rest-insts)])
       (match next-inst
         [(? symbol?)
          (values insts
                 (cons (Label-entry next-inst
                                    insts)
                       labels))]

         [_
          (values (cons (Instruction next-inst))
                  labels)]))]))
