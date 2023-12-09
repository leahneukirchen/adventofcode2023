(import :std/srfi/1
        :std/srfi/13
        :std/iter
        :std/misc/ports
        :std/pregexp
        :std/sugar)
(export main)
  
(defvalues (steps graph)
  (match (read-file-lines "day08")
    ([steps _ . nodes]
     (values (apply circular-list (string->list steps))
             (list->hash-table
              (for/collect (line nodes)
                (match (pregexp-split "[ =(),]+" line)
                  ([from left right]
                   (cons from (cons left right))))))))))

(def (step node direction)
  (match direction
    (#\L (car (hash-ref graph node)))
    (#\R (cdr (hash-ref graph node)))))

(def (traverse node steps n)
  (if (string-suffix? "Z" node)
    n
    (traverse (step node (car steps)) (cdr steps) (1+ n))))

(def (part1)
  (traverse "AAA" steps 0))

(def (part2)
  (chain (hash-keys graph)
    (filter (cut string-suffix? "A" <>) <>)
    (map (cut traverse <> steps 0) <>)
    (apply lcm <>)))

(def (main . args)
  (displayln (part1))                   ; 18727
  (displayln (part2)))                  ; 18024643846273
