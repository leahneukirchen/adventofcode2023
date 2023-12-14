(import :std/srfi/1
        :std/iter
        :std/misc/ports
        :std/pregexp
        "./advent.ss")
(export main)

(def (char-matrix lines)
  (list->vector (map string->vector lines)))

(def (matrix-ref matrix y x)
  (vector-ref (vector-ref matrix y) x))

(def (matrix-transpose matrix)
  (apply vector-map vector (vector->list matrix)))

(def (parse)
  (for/collect (pattern (pregexp-split "\n\n" (read-file-string "day13")))
    (char-matrix (pregexp-split "\n" pattern))))

(def (find-row matrix speck)
  (let/esc return
    (for (y (in-range 0 (1- (vector-length matrix))))
      (when (= speck (for/foldl (+ 0) (n (iota (vector-length matrix)))
                       (if (or (>= (+ 1 y n) (vector-length matrix))
                               (< (- y n) 0))
                         0
                         (for/foldl (+ 0) ((a (vector-ref matrix (+ 1 y n)))
                                           (b (vector-ref matrix (- y n))))
                           (if (eq? a b) 0 1)))))
        (return (1+ y))))
    #f))

(def (solve speck)
  (for/foldl (+ 0) (pattern (parse))
    (cond
      ((find-row pattern speck) => (cut * 100 <>))
      ((find-row (matrix-transpose pattern) speck) => identity)
      (else 0))))

(def (main . args)
  (displayln (solve 0))                 ; 37025
  (displayln (solve 1)))                ; 32854
