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

(provide spawn-task ls-tasks rm-task rm-all-tasks run-tasks spawn-timed-task clear-timed-tasks
         time-now print-error task-running?)


(define task-list '())  ; alist of tasks - maintained in sorted order


(struct timed-task (time thunk))

; a priority queue of timed tasks
(define timed-tasks
  (let ([<=? (lambda (t1 t2) (<= (timed-task-time t1)
                                 (timed-task-time t2)))])
    (make-queue <=?)))

(define (clear-timed-tasks) (queue-erase! timed-tasks))

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

(define (spawn-task thunk . args)
  (let ([name (if (null? args) (string->symbol (symbol->string (gensym))) (car args))])
    (rm-task name)      ; incase it already exists - replace it
    (set! task-list  (sort (cons (cons name thunk) task-list)
                           #:key (lambda (l) (symbol->string (car l)))
                           string<?))))

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
  (set! task-list (remove name task-list (lambda (a b) (eq? a (car b))))))

;; StartFunctionDoc-en
;; rm-all-tasks
;; Returns: void
;; Description:
;; Removes all task from the tasklist, including the every-frame task.
;; Example:
;; (rm-all-tasks) 
;; EndFunctionDoc    

(define (rm-all-tasks)
  (set! task-list '()))

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
  (for-each (lambda (t)
              (printf "task: ~a ~a~%" (car t) (cdr t)))
            task-list))


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

(define (task-running? t)
  (if (assoc t task-list) #t #f))


(define (thunk? t) (let ([arity (procedure-arity t)])
                     (or (eq? arity 0)
                         (and (list? arity) (eq? (car arity) 0)))))

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
        (queue-insert! timed-tasks (timed-task time thunk)))

(define (print-error e)
  (when (exn? e)
    (printf "~a ~n" (exn-message e))
    (printf "call stack:~n")
    (for ([c (continuation-mark-set->context
               (exn-continuation-marks e))])
      (if (cdr c)
        (printf "~a [line ~a in ~a]~n"
                (car c) (srcloc-line (cdr c)) (srcloc-source (cdr c)))
        (printf "~a~n" (car c))))))

(define (run-tasks)

  (for ([task (in-list task-list)])
       (with-handlers ([exn:fail?
                         ;; handle errors by reporting and removing task in error
                         (lambda (e)
                           (printf "Error in Task '~a - Task removed.~%"
                                   (car task))
                           (rm-task (car task))
                           (print-error e))])
         (unless (call-task (cdr task))
           (rm-task (car task)))))

  ; do the timed tasks
  (let loop ()
    (unless (or (queue-empty? timed-tasks)
                (< (time-now)
                   (timed-task-time (queue-top timed-tasks))))
      (let ([timed-task (queue-top timed-tasks)])
        (queue-delete-top! timed-tasks)
        (with-handlers ([exn:fail?
                          (lambda (e)
                            (printf "Error in Timed Task: ~%")
                            (print-error e))])
                       (call-task (timed-task-thunk timed-task)))
        (loop)))))


