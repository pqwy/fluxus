#lang racket

(require "tasks.ss" "building-blocks.ss")

(provide spawn restart-after restart-next-frame trap)


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
  (define (continuation . args)  ; Name it.
    (call-with-prompt (lambda () (apply k args))))
  continuation)

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


;; Break on through to the other side!
;;
(define (propagate-state thunk)
  
  (define prim (current-grab-target))

  (if prim

    ;; If we are in a context of a grab, reinstall `with-primitive' on the other
    ;; side of the prompt frame, where it is captured with the continuation.
    (lambda () (with-primitive prim (thunk)))

    ;; Similarly, if this is not a grab, (re-) install `with-state' inside the
    ;; prompt.
    (let ([state (get-ogl-state)])
      (lambda () (with-state
                   (apply-saved-ogl-state state)
                   (thunk))))))


(define current-frame-stuff (make-parameter #f))

;; Take the thunk and just run it. Preparing stuff for possible escape and
;; reentry.
;;
(define (run-restartable-thunk thunk)
  ;; Everything on the other side of the prompt is captured. Everything else is
  ;; not.
  (parameterize ([current-frame-stuff #f])
    (call-with-prompt (propagate-state thunk))))


(define-syntax-rule
  (spawn b b1 ...)
  (run-restartable-thunk (lambda () b b1 ...)))


;; End task and reschedule the rest of computation after given number of seconds: a wait.
;;
(define (restart-after sec)
  (trap (lambda (k)
          (let ([cfs (current-frame-stuff)])
            (when cfs (rm-task (car cfs))))
          (spawn-timed-task (+ (time-now) sec) k))))

;; Reschedule on the next frame. Has a fast-path for repeatedly doing so.
;;
(define (restart-next-frame)
  (trap (lambda (k)
          (let ([cfs (current-frame-stuff)])
            (if cfs (set-box! (cdr cfs) k)
              (let ([new-cfs (cons (gensym 'restartable-task-) (box k))])

                (define (restartable-task-runner) ; Name it.
                  (parameterize ([current-frame-stuff new-cfs])
                    ((unbox (cdr new-cfs)))))

                (spawn-task restartable-task-runner (car new-cfs))))))))


