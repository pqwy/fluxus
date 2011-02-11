#lang racket

(require "tasks.ss" "building-blocks.ss")

(provide spawn restart-after trap)


;; Our very own prompt tag. Not to interfere with exceptions or other
;; continuations.
;;
(define tag (make-continuation-prompt-tag 'restartable-boundary))

;; Set the prompt. The abort-handler does not reinstall the it, so rescheduling
;; operations happen in the dynamic extent of whatever was there previously.
;;
(define (call-with-prompt f)
  (call-with-continuation-prompt
    f tag
    (lambda (return-thunk) (return-thunk))))

(define (graft-prompt k)
  (lambda args
    (call-with-prompt (lambda () (apply k args)))))

;; Pretty much the classical `shift': capture the continuation delimited by our
;; prompt, abort it, and apply the handler to the continuation in a tail
;; context. The continuation is wrapped to reinstall the prompt when invoked.
;;
(define (trap f)
  (unless (continuation-prompt-available? tag)
    (error 'trap "must be called from within spawn"))

  (call-with-composable-continuation
    (lambda (k) (abort-current-continuation tag
                  (lambda () (f (graft-prompt k)))))
    tag))

;; End task and reschedule the rest of computation after given number of seconds: a wait.
;;
(define (restart-after sec)
  (trap (lambda (k) (spawn-timed-task (+ (time-now) sec) k))))

;; End task and reschedule the rest of computation for the next frame:
;; skip-frame.

;; -- nope. what if we were invoked as a fixed task?
; (define (restart-next-frame)
;   (let ([key (gensym 'restartable-continuation-)])
;     (trap (lambda (k)
;             (spawn-task
;               (lambda () (rm-task key) (k))
;               key)))))


;; Take the thunk and just run it. Preparing stuff for possible escape and
;; reentry.
;;
(define (run-restartable-thunk thunk)

  (define prim (current-grab-target))

  ;; If we are in a context of a grab, reinstall `with-primitive' on the other
  ;; side of the prompt frame, where it is captured with the continuation.
  (define (run-preserving-grab)
    (call-with-prompt
      (lambda () (with-primitive prim (thunk)))))

  ;; Similarly, if this is not a grab, (re-) install `with-state' inside the
  ;; prompt.
  (define (run-preserving-state)
    (let ([state (get-ogl-state)])
      (call-with-prompt
        (lambda () (with-state
                     (apply-saved-ogl-state state)
                     (thunk))))))

  ((if prim run-preserving-grab run-preserving-state)))


(define-syntax-rule
  (spawn b b1 ...)
  (run-restartable-thunk (lambda () b b1 ...)))

