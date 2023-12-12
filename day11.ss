(import :std/srfi/1
        :std/iter
        :std/misc/number
        :std/misc/list-builder
        :std/misc/ports
        :std/sugar)
(export main)

(def (char-matrix file)
  (list->vector (map (lambda (line) (string->vector line))
                     (read-file-lines file))))

(def (matrix-ref matrix y x)
  (vector-ref (vector-ref matrix y) x))

(def (find-galaxies matrix)
  (with-list-builder (push!)
    (for* ((y (in-range 0 (vector-length matrix)))
           (x (in-range 0 (vector-length (vector-ref matrix y)))))
      (when (equal? (matrix-ref matrix y x) #\#)
        (push! [y x])))))

(def (range-incl from to)
  (if (< to from)
    (range-incl to from)
    (iota (- to from -1) from)))

(def (main . args)
  (let* ((matrix (char-matrix "day11"))
         (galaxies (find-galaxies matrix))
         (missing-rows (lset-difference
                        =
                        (range-incl (apply min (map first galaxies))
                                    (apply max (map first galaxies)))
                        (map first galaxies)))
         (missing-cols (lset-difference
                        =
                        (range-incl (apply min (map second galaxies))
                                    (apply max (map second galaxies)))
                        (map second galaxies))))

    (def (diff a b factor)
      (match* (a b)
        (([ya xa] [yb xb])
         (+ (abs (- ya yb))
            (* (1- factor)
               (length (lset-intersection =
                                          (cdr (range-incl ya yb))
                                          missing-rows)))
            (abs (- xa xb))
            (* (1- factor)
               (length (lset-intersection =
                                          (cdr (range-incl xa xb))
                                          missing-cols)))))))

    (def p1 0)
    (def p2 0)
    (for* ((a galaxies)
           (b galaxies))
      (increment! p1 (diff a b 2))
      (increment! p2 (diff a b 1000000)))
    ;; need to halve because we counted ordered pairs
    (pp (/ p1 2))                       ; 9918828
    (pp (/ p2 2))))                     ; 692506533832
