(import :std/srfi/1
        :std/srfi/113
        :std/srfi/128
        :std/misc/hash
        :std/misc/number
        :std/misc/ports
        :std/iter
        :std/pregexp
        "./advent.ss")
(export main)

(def (char-matrix file)
  (list->vector (map string->vector (read-file-lines file))))

(definline (matrix-ref matrix y x)
  (vector-ref (vector-ref matrix y) x))

(definline (matrix-ref-cyclic matrix pos)
  (with ([y x] pos)
    (matrix-ref matrix
                (modulo y (vector-length matrix))
                (modulo x (vector-length matrix)))))

(def (find-S matrix)
  (let/esc return
    (for* ((y (in-range 0 (vector-length matrix)))
           (x (in-range 0 (vector-length (vector-ref matrix y)))))
      (when (equal? (matrix-ref matrix y x) #\S)
        (return [y x])))
    #f))

(def matrix (char-matrix "day21"))

(def seen (set (make-default-comparator)))
(def depths (make-vector (+ 64 131 131 1) 0))

(def (traverse pos depth max-depth)
  (unless (= depth max-depth)
    (for (d [[-1 0] [1 0] [0 -1] [0 1]])
      (let (neigh (map + pos d))
        (when (not (eq? #\# (matrix-ref-cyclic matrix neigh)))
          (unless (set-contains? seen (cons depth neigh))
            (set-adjoin! seen (cons depth neigh))
            (increment! (vector-ref depths depth))
            (traverse neigh (1+ depth) max-depth)))))))

(def (magic x a0 a1 a2)
  (let ((b0 a0)
        (b1 (- a1 a0))
        (b2 (- a2 a1)))
    (+ b0 (* b1 x) (* x (/ (1- x) 2) (- b2 b1)))))

(def (main . args)
  (traverse (find-S matrix) 0 (+ 64 131 131 1))
  (displayln (vector-ref depths 63))     ; 3709
  (displayln (magic 202300
                    (vector-ref depths 64)
                    (vector-ref depths (+ 64 131))
                    (vector-ref depths (+ 64 131 131)))))  ; 617361073602319
