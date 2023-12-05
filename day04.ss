(import :std/srfi/1
        :std/srfi/133
        :std/misc/number
        :std/iter
        :std/sugar
        :std/pregexp)
(export main)

(def vector-ref-set! vector-set!)

(def (parse file)
  (call-with-input-file file
    (lambda (input)
      (list->vector
       (for/collect (line (in-input-lines input))
         (match (pregexp-split "[:|] +" line)
           ([_ winning numbers]
            (length (lset-intersection equal?
                                       (pregexp-split " +" winning)
                                       (pregexp-split " +" numbers))))))))))
           
(def (part1 wins)
  (def (score n)
    (if (= n 0)
      0
      (expt 2 (1- n))))
  (vector-fold + 0 (vector-map score wins)))

(def (part2 wins)
  (let (counts (make-vector (vector-length wins) 1))
    (for ((i (in-range 0 (vector-length wins))))
      (for ((j (in-range (+ i 1) (+ i 1 (vector-ref wins i)))))
        (increment! (vector-ref counts j) (vector-ref counts i))))
    (vector-fold + 0 counts)))

(def (main . args)
  (let (data (parse "day04"))
    (displayln (part1 data))            ; 17782
    (displayln (part2 data))))          ; 8477787