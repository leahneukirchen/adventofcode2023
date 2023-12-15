(import :std/srfi/1
        :std/srfi/133
        :std/iter
        :std/misc/list
        :std/misc/ports
        :std/sugar
        "./advent.ss")
(export main)

(def (char-matrix lines)
  (list->vector (map string->vector lines)))

(definline (matrix-ref matrix y x)
  (vector-ref (vector-ref matrix y) x))

(definline (matrix-ref-set! matrix y x value)
  (vector-set! (vector-ref matrix y) x value))

(def (matrix-rotate! matrix)
  ;; reverse
  (vector-reverse! matrix)
  ;; and transpose in-place
  (for* ((y (in-range 0 (vector-length matrix)))
         (x (in-range 0 y)))
      (let ((n (matrix-ref matrix x y))
            (m (matrix-ref matrix y x)))
        (set! (matrix-ref matrix x y) m)
        (set! (matrix-ref matrix y x) n)))
  matrix)

(def (matrix-copy matrix)
  (vector-map vector-copy matrix))

(def (tilt-north! matrix)
  (for* ((y (in-range 1 (vector-length matrix)))
         (x (in-range 0 (vector-length (vector-ref matrix y))))
         (z (in-range (1- y) -1)))
    (when (and (eq? (matrix-ref matrix  z     x) #\.)
               (eq? (matrix-ref matrix (1+ z) x) #\O))
      (set! (matrix-ref matrix z x)      #\O)
      (set! (matrix-ref matrix (1+ z) x) #\.)))
  matrix)

(def (tilt-full! matrix)
  (for (_ (in-range 4))
    (tilt-north! matrix)
    (matrix-rotate! matrix)))

(def (vector-count vector elt)
  (for/foldl (+ 0) ((e vector))
    (if (eq? e elt) 1 0)))

(def (score matrix)
  (for/foldl (+ 0) ((y matrix)
                    (v (in-range (vector-length matrix) 0)))
    (* v (vector-count y #\O))))

(def (hash-find-value h value)
  (let/esc return
    (for (((values k v) (in-hash h)))
      (if (eq? v value)
        (return k)))))

(def (main . args)
  (let (matrix (char-matrix (read-file-lines "day14")))
    (tilt-north! matrix)
    (displayln (score matrix))          ; 108144
    
    (def seen (make-hash-table))
    (def start #f)
    (def period #f)
    
    (let/esc break
      (for (n (in-range 0 1000000000))
        (let (s (matrix-copy matrix))
          (if (hash-get seen s)
            (begin
              (when start
                (set! period (- n start 1))
                (break))
              (set! start n)
              (hash-clear! seen))
            (hash-put! seen s n)))
        (tilt-full! matrix)))

    (chain (+ start (remainder (- 1000000000 start) period))
      (hash-find-value seen <>)
      score
      displayln)))                      ; 108404
