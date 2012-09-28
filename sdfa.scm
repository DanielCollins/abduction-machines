(define (state final?)
  (vector 'state final? '()))

(define (final? state)
  (vector-ref state 1))

(define (transitions state)
  (vector-ref state 2))

(define (transition state input)
  (let ((p (assv input (transitions state))))
    (if (pair? p)
        (cdr p)
        #f)))

(define (set-final! state f?)
  (vector-set! state 1 f?))

(define (set-transitions! state transitions)
  (vector-set! state 2 transitions))

(define (add-transition! source destination input)
  (let ((t (assv input (transitions source))))
    (if t
        (set-cdr! t destination)
        (set-transitions! source (cons (cons input destination)
                                       (transitions source)))))) 

(define (accepted? state input)
  (if (not state)
      #f
      (if (null? input)
          (final? state)
          (accepted? (transition state
                                 (car input))
                     (cdr input)))))

(define (generate state)
  (define (loop state output)
    (if (final? state)
        (begin (display output)
               (newline)))
    (map (lambda (a)
           (loop (cdr a) (cons (car a) output)))
         (transitions state)))
  (loop state '())
  'done)

(define (learn! old-state input)
  (if (null? input)
      (set-final! old-state #t) 
      (let ((t (transition old-state (car input))))
        (if t
            (learn! t (cdr input))
            (let ((new-state (state #f)))
              (add-transition! old-state new-state (car input))
              (learn! new-state (cdr input))))))) 



;; example DFA accepts { (0), (0 0), (0 0 0), ... }, rejects 1's
;
; (define root (state #t))
; (define rejecting (state #f))
; (add-transition! root root 0)
; (add-transition! root rejecting 1)
; (add-transition! rejecting rejecting 0)
; (add-transition! rejecting rejecting 1)
;

