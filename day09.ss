(import :std/srfi/1
        :std/iter
        :std/misc/ports
        :std/pregexp
        :std/sugar
        "./advent.ss")
(export main)

(def (scan op xs)
  (map op xs (cdr xs)))

(def (extrapolate xs)
  (let (diff (scan - xs))
    (if (every zero? diff)
      (last xs)
      (- (last xs) (extrapolate diff)))))

(def (solve trans)
  (for/foldl (+ 0) (line (in-file-lines "day09"))
     (extrapolate (trans (parse-list line)))))

(def (main . args)
  (displayln (solve identity))          ; 1789635132
  (displayln (solve reverse)))          ; 913
