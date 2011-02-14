#lang racket/base

(require "tasks.ss" "building-blocks.ss")

(provide spawn restart-after restart-next-frame trap)


;; Our very own prompt tag. Not to interfere with exceptions or other
;; continuations.
;;
(define tag (make-continuation-prompt-tag 'restartable-boundary))

;; Set the prompt. The abort-handler does not reinstall the it, so rescheduling
;; operations are not delimited.
;;
(define (call-with-prompt f)
  (call-with-continuation-prompt
    f tag
    (lambda (return-thunk) (return-thunk))))

;; Pretty much the classical `shift' operator: capture the continuation
;; delimited by our prompt, abort it, then and apply the handler to the
;; continuation in a tail context. The continuation is wrapped to reinstall the
;; prompt when invoked.
;;
(define (trap f)
  (unless (continuation-prompt-available? tag)
    (error 'trap "must be called from within spawn"))

  (call-with-composable-continuation
    (lambda (k) (abort-current-continuation tag
                  (lambda () (f (graft-prompt k)))))
    tag))

(define (graft-prompt k)
  (define (continuation . a)
    (call-with-prompt (lambda () (apply k a))))
  continuation)


;; Break on through to the other side, yeah.
;;
(define (propagate-state thunk)

  ;; If we are in a context of a grab, reinstall `with-primitive' on the other
  ;; side of the prompt frame, where it is captured with the continuation.
  (cond [(current-grab-target)
         => (lambda (prim) (with-primitive prim (thunk)))]

        ;; Similarly, if this is not a grab, (re-) install `with-state' inside
        ;; the prompt.
        [else (let ([state (get-ogl-state)])
                (with-state (apply-saved-ogl-state state)
                            (thunk)))]))


(define current-frame-stuff (make-parameter #f))

;; Take the thunk and just run it. Preparing stuff for possible escape and
;; reentry.
;;
(define (run-restartable-thunk thunk)

  ;; Everything on the other side of the prompt is captured. Everything else is
  ;; not. #f: remove if spawn-task'd.
  (parameterize ([current-frame-stuff #f])
    (call-with-prompt
      (lambda () (propagate-state thunk) #f)))

  (void))


(define-syntax-rule
  (spawn b b1 ...)
  (run-restartable-thunk (lambda () b b1 ...)))


;; End task and reschedule the rest of computation after given number of
;; seconds: a wait.
;;
(define (restart-after sec)
  (trap (lambda (k)
          (spawn-timed-task (+ (time-now) sec) k) #f)))

;; Reschedule on the next frame. Has a fast-path for repeatedly doing so.
;;
(define (restart-next-frame)
  (trap (lambda (k)
          (cond [(current-frame-stuff)
                 => (lambda (cfs) (set-box! (cdr cfs) k))]
                [else
                  (let ([cfs (cons (gensym 'restartable-task-) (box k))])

                    (define (restartable-task-runner)
                      (parameterize ([current-frame-stuff cfs])
                        ((unbox (cdr cfs)))))

                    (spawn-task restartable-task-runner (car cfs)))]))))

