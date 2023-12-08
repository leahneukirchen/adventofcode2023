(import :std/srfi/1
        :std/misc/ports
        :std/iter
        :std/pregexp)
(export main)

(def (int r)
  (inexact->exact (floor r)))

(def (races duration distance)
  (let (disc (- (* duration duration) (* 4 distance)))
    (- (int (/ (+ duration (sqrt disc)) 2))
       (int (/ (- duration (sqrt disc)) 2))
       (if (exact? (sqrt disc)) 1 0))))

(def data
  (for/collect (line (read-file-lines "day06"))
    (map string->number (cdr (pregexp-split " +" line)))))

(def (part1)
  (apply * (map races (first data) (second data))))

(def (part2)
  (def (squash numbers)
    (string->number (string-join (map number->string numbers) "")))
  (races (squash (first data))
         (squash (second data))))

(def (main . args)
  (displayln (part1))                   ; 170000
  (displayln (part2)))                  ; 20537782
