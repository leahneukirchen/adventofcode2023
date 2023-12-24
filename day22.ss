(import :std/srfi/1
        :std/misc/number
        :std/iter
        :std/sort
        :std/misc/ports
        :std/pregexp
        "./advent.ss")
(export main)

(def (brick-fall stack skip peaks)
  (let (falls 0)
    (for ((arr stack) (i (in-naturals)))
      (unless (= i skip)
        (with ([x1 y1 z1 x2 y2 z2] arr)
          (let (peak 0)
            (for* ((i (in-range x1 x2))
                   (j (in-range y1 y2)))
              (set! peak (max peak (vector-ref (vector-ref peaks i) j))))
            (for* ((i (in-range x1 x2))
                   (j (in-range y1 y2)))
              (vector-set! (vector-ref peaks i) j (+ peak z2 (- z1))))
            (vector-set! stack i [x1 y1 peak x2 y2 (+ peak z2 (- z1))])
            (when (< peak z1)
              (increment! falls))))))
    falls))

(def (peaks)
  (list->vector
   (list-tabulate 12 (lambda _ (make-vector 12 0)))))

(def (main . args)
  (def stack
    (list->vector
     (for/collect (line (in-file-lines "day22"))
       (map +
            (map string->number (pregexp-split "[~,]" line))
            '(0 0 0 1 1 1)))))

  (sort! stack (lambda (a b) (< (third a) (third b))))
  (brick-fall stack -1 (peaks))

  (def part1 0)
  (def part2 0)

  (for ((_ stack) (i (in-naturals)))
    (let (stack-copy (vector-map list-copy stack))
      (let (r (brick-fall stack-copy i (peaks)))
        (when (zero? r)
          (increment! part1))
        (increment! part2 r))))

  (displayln part1)                     ; 391
  (displayln part2))                    ; 69601
