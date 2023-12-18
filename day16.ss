(import :std/srfi/1
        :std/srfi/113
        :std/srfi/128
        :std/iter
        :std/misc/ports
        :std/sugar)
(export main)

(def (char-matrix file)
  (list->vector (map string->vector (read-file-lines file))))

(def (matrix-ref matrix y x)
  (vector-ref (vector-ref matrix y) x))

(def (beam matrix y x dir visited)
  (and (<= 0 y)
       (< y (vector-length matrix))
       (<= 0 x)
       (< x (vector-length (vector-ref matrix y)))
       (not (set-contains? visited (list y x dir)))
       (begin
         (set-adjoin! visited (list y x dir))
         (let (new-dir (match* ((matrix-ref matrix y x) dir)
                         ((#\. 'E) 'E)
                         ((#\/ 'E) 'N)
                         ((#\\ 'E) 'S)
                         ((#\| 'E) 'NS)
                         ((#\- 'E) 'E)

                         ((#\. 'W) 'W)
                         ((#\/ 'W) 'S)
                         ((#\\ 'W) 'N)
                         ((#\| 'W) 'NS)
                         ((#\- 'W) 'W)

                         ((#\. 'N) 'N)
                         ((#\/ 'N) 'E)
                         ((#\\ 'N) 'W)
                         ((#\| 'N) 'N)
                         ((#\- 'N) 'EW)

                         ((#\. 'S) 'S)
                         ((#\/ 'S) 'W)
                         ((#\\ 'S) 'E)
                         ((#\| 'S) 'S)
                         ((#\- 'S) 'EW)))
           (match new-dir
             ('N (beam matrix (1- y) x new-dir visited))
             ('S (beam matrix (1+ y) x new-dir visited))
             ('W (beam matrix y (1- x) new-dir visited))
             ('E (beam matrix y (1+ x) new-dir visited))
             ('NS
              (beam matrix (1- y) x 'N visited)
              (beam matrix (1+ y) x 'S visited))
             ('EW
              (beam matrix y (1+ x) 'E visited)
              (beam matrix y (1- x) 'W visited))))
         visited)))

(def (solve matrix y x dir)
  (chain matrix
    (beam <> y x dir (set (make-default-comparator)))
    (set-map (make-default-comparator) (cut take <> 2) <>)
    set-size))

(def (part1 matrix)
  (solve matrix 0 0 'E))

(def (part2 matrix)
  (apply max
    (append
     (for/collect (y (in-range 0 (vector-length matrix)))
       (solve matrix y 0 'E))
     (for/collect (y (in-range 0 (vector-length matrix)))
       (solve matrix y (1- (vector-length (vector-ref matrix y))) 'W))
     (for/collect (x (in-range 0 (vector-length (vector-ref matrix 0))))
       (solve matrix 0 x 'S))
     (for/collect (x (in-range 0 (vector-length (vector-ref matrix 0))))
       (solve matrix (1- (vector-length matrix)) x 'N)))))

(def (main . args)
  (let (matrix (char-matrix "day16"))
    (displayln (part1 matrix))          ; 8551
    (displayln (part2 matrix))))        ; 8754
