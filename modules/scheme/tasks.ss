;; Provide a named safe tasks
;;
;; Currently tasks are limited to "thunks" but may be extended to support continuations
;;
;; Tasks:
;; * execute once per graphic frame
;; * are called in lexigraphical order by name
;; * have unique names and if the same name is used the old task is
;;   removed prior to the new task being added
;; * a task that returns #f will remove itself after executing
;;
;; run-tasks puts itself on the frame-hooks list defined in scratchpad
;; LATER: if task returns a thunk, then that thunk will replace the old one
;; LATER: if a task returns a continuation, then that continuation will be called next frame
;;
;; (ls-tasks)
;; (spawn-task thunk)
;; (spawn-task thunk 'name)
;; (rm-task 'name)

;; StartSectionDoc-en
;; scratchpad
;; Functions available as part of the fluxus scratchpad.
;; Example:
;; EndSectionDoc 

;; StartSectionDoc-pt
;; scratchpad
;; Funções disponíveis como parte do scratchpad fluxus.
;; Exemplo:
;; EndSectionDoc

#lang racket/base
(require "time.ss" "pqueue.ss")

(provide spawn-task ls-tasks rm-task rm-all-tasks run-tasks spawn-timed-task after
         clear-timed-tasks time-now print-error task-running?)


;; The mapping between names and tasks. Hash, because this could frequently
;; exercise random access.
(define tasks #hasheq())

; A (mutable) priority queue of timed tasks.
(define timed-tasks
  (let ([<=?  (lambda (t1 t2) (<= (car t1) (car t2)))])
    (make-queue <=?)))

;; StartFunctionDoc-en
;; spawn-task
;; Returns: void
;; Description:
;; Launches a new per-frame task, a tasks:
;; * execute once per graphic frame
;; * are called in lexigraphical order by name
;; * have unique names and if the same name is used the old task is
;;   removed prior to the new task being added
;; * a task that returns #f will remove itself after executing
;; (every-frame (build-cube)) is equivalent to (spawn-task (lambda () (build-cube)) 'every-frame-task)
;; Example:
;; (spawn-task (lambda () (draw-torus)) 'torus-task)
;; (rm-task 'torus-task)
;; EndFunctionDoc    

(define (spawn-task thunk [name (gensym 'task-)])
  (set! tasks (hash-set tasks name thunk))
  name)

;; StartFunctionDoc-en
;; rm-task
;; Returns: void
;; Description:
;; Removes a task from the tasklist
;; Example:
;; (spawn-task (lambda () (draw-torus)) 'torus-task) ; add a task
;; (rm-task 'torus-task) ; remove it again
;; EndFunctionDoc    

(define (rm-task name)
  (set! tasks (hash-remove tasks name)))

;; StartFunctionDoc-en
;; rm-all-tasks
;; Returns: void
;; Description:
;; Removes all task from the tasklist, including the every-frame task.
;; Example:
;; (rm-all-tasks) 
;; EndFunctionDoc    

(define (rm-all-tasks)
  (set! tasks #hasheq()))

;; StartFunctionDoc-en
;; ls-tasks
;; Returns: void
;; Description:
;; Prints a list of current a tasks
;; Example:
;; (spawn-task (lambda () (draw-torus)) 'torus-task) ; add a task
;; (ls-tasks)
;; (rm-task 'torus-task)
;; EndFunctionDoc

(define (ls-tasks)
  (for ([(name task) tasks])
    (printf "task: ~a ~a~%" name task))

  (let ([c (queue-count timed-tasks)])
    (unless (zero? c)
      (printf "~%* scheduled tasks: ~a~%" c))))


;; StartFunctionDoc-en
;; task-running? task-name-symbol
;; Returns: boolean
;; Description:
;; Checks if a task is running.
;; Example:
;; (spawn-task (lambda () (draw-torus)) 'torus-task)
;; (display (task-running? 'torus-task))(newline)
;; (rm-task 'torus-task)
;; (display (task-running? 'torus-task))(newline)
;; EndFunctionDoc

(define (task-running? name)
  (hash-has-key? tasks name))


(define (thunk? t) (and (procedure? t) (procedure-arity-includes? t 0)))

(define (call-task task)
  (cond [(thunk? task) (task)]
;;      [(continuation? task) (task 'resume)]
        [else (error "Non-thunk or continuation passed to call-task")]))

;; StartFunctionDoc-en
;; spawn-timed-task time thunk
;; Returns: void
;; Description:
;; Launches a new timed task, which will happen in the future, 
;; on the frame that the time specifies. Use (time-now) rather than (time) to
;; obtain the time. I need to sort that out.
;; Example:
;; (spawn-timed-task (+ (time-now) 10) ; schedule a task 10 seconds from now
;;     (lambda () (display "hello future!") (newline)))
;; EndFunctionDoc    

(define (spawn-timed-task time thunk)
  (queue-insert! timed-tasks (cons time thunk)))

(define-syntax-rule
  (after seconds b b1 ...)
  (spawn-timed-task (+ (time-now) seconds)
                    (lambda () b b1 ...)))


(define (clear-timed-tasks) (queue-erase! timed-tasks))



(define (print-error e)
  (cond [(exn? e)
         (printf "[ ~a ]~n~n" (exn-message e))
         (printf "call stack:~n")
         (for ([c (continuation-mark-set->context
                    (exn-continuation-marks e))])
           (if (cdr c)
             (printf "  ~a [line ~a in ~a]~n"
                     (car c) (srcloc-line (cdr c)) (srcloc-source (cdr c)))
             (printf "  ~a~n" (car c))))
         (newline)]
        [else
         (printf "** unrecognized error: ~a **~n" e)]))


;; Handle just about anything, else we can get into a nasty loop where
;; unrecognized exceptionas are thrown every frame.
;;
(define (not-exn:break? e) (not (exn:break? e)))

(define (run-tasks)

  ;; Iterate through a snapshot of (recurrent) tasks. If any task starts a new
  ;; one, the iteration is safe, and the new task will appear the next time
  ;; around.
  (for ([(name task) (in-hash tasks)])
    (with-handlers ([not-exn:break?
                      ;; handle errors by reporting and removing task in error
                      (lambda (e)
                        (rm-task name)
                        (printf "Error in Task '~a - Task removed.~%~%" name)
                        (print-error e))])
      (unless (call-task task)
        (rm-task task))))

  ;; Repeatedly dequeue the earliest timed task, until no more. Changes the queue
  ;; in-place, and any recursively started timed tasks are added immediately.
  ;; This means they could run during the same loop.
  (let roll ()
    (unless (or (queue-empty? timed-tasks)
                (< (time-now) (car (queue-top timed-tasks))))
      (let ([task (cdr (queue-top timed-tasks))])
        (queue-delete-top! timed-tasks)
        (with-handlers ([not-exn:break?
                          (lambda (e)
                            (printf "Error in Timed Task: ~%")
                            (print-error e))])
          (call-task task))
        (roll)))))


; RACKET THREADS. THEY DON'T WORK.
;
; ;; A random number. It was obtained by a throw of an unbiased dice.
; (define tasks-wait-period 5)

; (define (run-tasks)
;   (let ([r (timeout-after tasks-wait-period
;                           (lambda () 'dead) raw-run-tasks)])
;     (when (eq? r 'dead)
;       (clear-timed-tasks)
;       (rm-all-tasks)
;       (error 'run-tasks
;              "Per-frame tasks took more than ~a seconds and have been reset."
;              tasks-wait-period))))



; (define (timeout-after sec to-handler thunk)
;   (let* ([main (current-thread)]
;          [killed #f]
;          [guardian
;            (thread (lambda ()
;                      (sleep sec)
;                      (set! killed #t)
;                      (break-thread main)))])

;     (with-handlers ([exn:break?
;                       (lambda (e)
;                         (if killed
;                           (begin (printf ">> KILLED~%") (to-handler))
;                           (raise e)))])
;       (begin0
;         (thunk)
;         (kill-thread guardian)))))


