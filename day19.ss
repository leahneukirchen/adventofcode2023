(import :std/srfi/1
        :std/srfi/128
        :std/misc/hash
        :std/misc/number
        :std/iter
        :std/misc/ports
        :std/pregexp
        "./advent.ss")
(export main)

(defvalues (workflows parts)
  (with ([workflows parts] (pregexp-split "\n\n" (read-file-string "day19")))
    (values
     (list->hash-table
      (for/collect (line (pregexp-split "\n" workflows))
        (with ([_ label rules] (pregexp-match "(.*?)\\{(.*)\\}" line))
          (cons label
                (for/collect (rule (pregexp-split "," rules))
                  (match (pregexp-match "(([xmas])([<>])(\\d+):)?(\\w+)" rule)
                    ([_ _ r "<" n target]
                     ['< r (string->number n) target])
                    ([_ _ r ">" n target]
                     ['> r (string->number n) target])
                    ([_ _ _ #f #f target]
                     ['> "x" -1 target])))))))
     (for/collect (line (pregexp-split "\n" parts))
       (with ([_ x m a s] (pregexp-match
                           "\\{x=(\\d+),m=(\\d+),a=(\\d+),s=(\\d+)\\}" line))
         (plist->hash-table
          ["x" (string->number x)
           "m" (string->number m)
           "a" (string->number a)
           "s" (string->number s)]))))))

(def (run name part)
  (match name
    ("A" #t)
    ("R" #f)
    (name
     (let/esc return
       (for ([cmp r n target] (hash-get workflows name))
         (when ((if (eq? cmp '>) > <) (hash-get part r) n)
           (return (run target part))))))))

(def (part1)
  (for/foldl (+ 0) (part parts)
    (if (run "in" part)
      (fold + 0 (hash-values part))
      0)))

(def (part2)
  (def p2 0)
  (def (backtrack target b)
    (for ((values label workflow) (in-hash workflows))
      (for ((rule workflow)
            (i (in-naturals)))
        (when (equal? (fourth rule) target)
          (let ((bounds (hash-copy b)))
            (for (j (in-range i -1))
              (with ([cmp r n _] (list-ref workflow j))
                (when (> n 0)
                  (set! (hash-ref bounds r)
                    (with ([lo hi] (hash-ref bounds r))
                      (if (= i j)
                        (if (eq? cmp '>)
                          [(max (1+ n) lo) hi]
                          [lo (min (1- n) hi)])
                        (if (eq? cmp '<)
                          [(max n lo) hi]
                          [lo (min n hi)])))))
                ))
            (if (equal? label "in")
              (increment! p2 (apply * (map (match <> ([lo hi] (1+ (- hi lo))))
                                           (hash-values bounds))))
              (backtrack label bounds)))))))
  (backtrack "A" (plist->hash-table
                  ["x" [1 4000]
                   "m" [1 4000]
                   "a" [1 4000]
                   "s" [1 4000]]))
  p2)

(define (main . args)
  (displayln (part1))                   ; 409898
  (displayln (part2)))                  ; 113057405770956
