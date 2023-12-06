(import :std/srfi/1
        :std/misc/ports
        :std/misc/list
        :std/misc/list-builder
        :std/misc/queue
        :std/iter
        :std/sugar
        :std/pregexp)
(export main)

(def (list->queue xs)
  (let (q (make-queue))
    (for ((x xs))
      (enqueue! q x))
    q))

(define (split-by xs x)
  (foldr (lambda (element next)
           (if (equal? element x)
             (cons '() next)
             (cons (cons element (car next)) (cdr next))))
         (list '()) xs))

(def (parse-number-list str)
  (map string->number (pregexp-split " +" str)))

(def seeds #f)
(def mappings '())

(def (parse file)
  (set! mappings
    (with-list-builder (put!)
      (for ((para (split-by (read-file-lines file) "")))
        (if (string-prefix? "seeds:" (car para))
          (set! seeds (parse-number-list
                       (second (pregexp-split ": " (car para)))))
          (put! (for/collect ((line (cdr para)))
                  (parse-number-list line))))))))

(def (lookup mapping n)
  (let/esc return
    (for ([to from len] mapping)
      (when (<= from n (+ from len))
        (return (+ to (- n from)))))
    n))                                 ; default

(def (range-intersect a b)
  (if (and (< (car a) (cdr b)) (< (car b) (cdr a)))
    (cons (max (car a) (car b)) (min (cdr a) (cdr b)))
    #f))

(def (range-subtract a b)
  (with-list-builder (put!)
    (when-let (i (range-intersect a b))
      (when (< (car a) (car i))
        (put! (cons (car a) (car i))))
      (when (< (cdr i) (cdr a))
        (put! (cons (cdr i) (cdr a)))))))

(def (lookup-ranges mapping ranges)
  (def q (list->queue ranges))
  (with-list-builder (put!)
    (until (queue-empty? q)
      (let (r (dequeue! q))
        (let/esc break
          (for ([to from len] mapping)
            (when-let (i (range-intersect r (cons from (+ from len))))
              (put! (cons (+ to (- (car i) from))
                          (+ to (- (cdr i) from))))
              (for (new (range-subtract r (cons from (+ from len))))
                (enqueue! q new))
              (break)))
          (put! r))))))
      
(def (part1)
  (for/fold (p1 +inf.0) (seed seeds)
    (min p1 (fold lookup seed mappings))))

(def (part2)
  (chain seeds
    (group-n-consecutive 2 <>)
    (map (match <> ([a l] (cons a (+ a l)))) <>)
    (fold lookup-ranges <> mappings)
    (map car <>)
    (apply min <>)))

(def (main . args)
  (parse "day05")
  (displayln (part1))                     ; 662197086
  (displayln (part2)))                    ; 52510809
