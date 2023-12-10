(import :std/srfi/1
        :std/iter
        :std/misc/ports
        :std/pregexp
        :std/sugar)
(export main)

(def (scan op xs)
  (map op xs (cdr xs)))

(def (extrapolate xs)
  (let (diff (scan - xs))
    (if (every zero? diff)
      (last xs)
      (- (last xs) (extrapolate diff)))))

(def (solve trans)
  (call-with-input-file "day09"
    (lambda (input)
      (for/fold (sum 0) (line (in-input-lines input))
        (+ sum (extrapolate
                (trans (map string->number (pregexp-split " +" line)))))))))

(def (main . args)
  (displayln (solve identity))          ; 1789635132
  (displayln (solve reverse)))          ; 913
