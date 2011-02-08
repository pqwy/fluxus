#lang racket/base

(require racket/match)

(provide make-queue queue-top queue-empty?
         queue-insert! queue-delete-top! queue-erase!)


(struct queue (head <=?) #:mutable)

(struct node (item left right) #:mutable)


(define (make-queue <=? . as)
  (let ([q (queue #f <=?)])
    (for ([x as]) (queue-insert! q x))
    q))

(define (queue-erase! q) (set-queue-head! q #f))

(define (queue-top q) (node-item (queue-head q)))

(define (queue-empty? q) (not (queue-head q)))

(define (queue-insert! q item)
  (let ([head (queue-head q)] [<=? (queue-<=? q)])
;   (match-let ([(queue head <=?) q])
    (set-queue-head! q (union-nodes! <=? (node item #f #f) head))))

(define (queue-delete-top! q)
  (let* ([node (queue-head q)] [<=? (queue-<=? q)]
         [left (node-left node)] [right (node-right node)])
;   (match-let ([(queue (node _ left right) <=?) q])
    (set-queue-head! q (union-nodes! <=? left right))))

(define (union-nodes! <=? a b)
  (cond [(not b) a]
        [(not a) b]
        [(<=? (node-item a) (node-item b))
         (set-node-left! a (union-nodes! <=? b (node-right a)))
         (set-node-right! a (node-left a))
         a]
        [else
         (set-node-left! b (union-nodes! <=? a (node-right b)))
         (set-node-right! b (node-left b))
         b]))

;   (match* (a b)
;     [((node ia la ra) (node ib lb rb))
;      (cond [(<=? ia ib)
;             (set-node-left! a (union-nodes! <=? b ra)) (set-node-right! a la) a]
;            [else
;             (set-node-left! b (union-nodes! <=? a rb)) (set-node-right! b lb) b])]
;     [(a #f) a]
;     [(#f b) b]))


