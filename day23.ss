(import :std/srfi/1
        :std/srfi/113
        :std/srfi/128
        :std/iter
        :std/misc/repr
        :std/misc/number
        :std/misc/ports
        :std/sugar)
(export main)

(defrule (one-of args ...)
  (lambda (x) (case x ((args ...) #t) (else #f))))

(def (char-matrix file)
  (list->vector (map string->vector (read-file-lines file))))

(def (matrix-ref matrix y x)
  (if (and (< -1 y (vector-length matrix))
           (< -1 x (vector-length (vector-ref matrix y))))
    (vector-ref (vector-ref matrix y) x)
    #f))

(def (vector-find xs e)
  (let/esc return
    (for ((x xs) (i (in-naturals)))
      (when (equal? x e)
        (return i)))
    #f))

(def part1 0)
(def (traverse matrix y x stop len seen)
  (if (equal? [y x] stop)
    (begin
      (displayln len)
      (set! part1 (max part1 len)))
    (unless (set-contains? seen [y x])
      (let (seen (set-adjoin seen [y x]))
        (when ((one-of #\. #\v) (matrix-ref matrix (1+ y) x))
          (traverse matrix (1+ y) x stop (1+ len) seen))
        (when ((one-of #\. #\^) (matrix-ref matrix (1- y) x))
          (traverse matrix (1- y) x stop (1+ len) seen))
        (when ((one-of #\. #\>) (matrix-ref matrix y (1+ x)))
          (traverse matrix y (1+ x) stop (1+ len) seen))
        (when ((one-of #\. #\<) (matrix-ref matrix y (1- x)))
          (traverse matrix y (1- x) stop (1+ len) seen))))))

;; thx https://gist.github.com/ke-hermann/279f352829cd590d61104c27cac59bdc

(def (main . args)
  (let* ((matrix (char-matrix "day23"))
         (start [0 (vector-find (vector-ref matrix 0) #\.)])
         (stop [(1- (vector-length matrix))
                (vector-find (vector-ref matrix (1- (vector-length matrix))) #\.)]))
    (traverse matrix (first start) (second start) stop 0 (set (make-default-comparator)))
    (displayln part1)

    (def neighbors (hash))
    (def intersections [start stop])
    (def graph (hash))
    
    (def (intersect-dist cur dist seen)
      (if (member cur intersections)
        [cur dist]
        (let (next (let/esc break
                     (for (p (hash-ref neighbors cur []))
                       (unless (set-contains? seen p)
                         (break p)))
                     #f))
          (intersect-dist next
                          (1+ dist)
                          (set-adjoin seen cur)))))

    (def (bfs node score seen)
      (if (equal? node stop)
        score
        (apply max 0
               (filter (lambda (x) (not (void? x)))
                       (for/collect ([p dist] (hash-ref graph node))
                         (unless (set-contains? seen p)
                           (bfs p (+ score dist) (set-adjoin seen node))))))))
  
    (for* ((y (in-range 0 (vector-length matrix)))
           (x (in-range 0 (vector-length (vector-ref matrix y)))))
      (when ((one-of #\. #\> #\< #\v #\^) (matrix-ref matrix y x))
        (let (ec 0)
          (for ([dx dy] [[0 -1] [0 1] [1 0] [-1 0]])
            (when ((one-of #\. #\> #\< #\v #\^) (matrix-ref matrix (+ y dy) (+ x dx)))
              (increment! ec)
              (hash-put! neighbors [y x]
                         (cons [(+ y dy) (+ x dx)]
                               (or (hash-get neighbors [y x]) [])))))
          (when (>= ec 3)
            (pp (vector 'ec y x ec (matrix-ref matrix y x)))
            (set! intersections (cons [y x] intersections))))))
    
    (for* ((i intersections)
           (n (hash-ref neighbors i)))
      (hash-put! graph i (cons (intersect-dist n 1
                                               (set (make-default-comparator) i))
                               (or (hash-get graph i) []))))
    
    (displayln (bfs start 0 (set (make-default-comparator) start))) ; 6470
    ))