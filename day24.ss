(import :std/srfi/1
        :std/srfi/113
        :std/srfi/128
        :std/iter
        :std/misc/number
        :std/misc/ports
        :std/sugar
        :std/pregexp
        "./advent.ss")
(export main)

; thx to https://raw.githubusercontent.com/girishji/AoC2023/main/day24.solution.py

;; https://rosettacode.org/wiki/Reduced_row_echelon_form#Scheme
(define (reduced-row-echelon-form matrix)
  (define (clean-down matrix from-row column)
    (cons (car matrix)
          (if (zero? from-row)
              (map (lambda (row)
                     (map -
                          row
                          (map (lambda (element)
                                 (/ (* element (list-ref row column))
                                    (list-ref (car matrix) column)))
                               (car matrix))))
                   (cdr matrix))
              (clean-down (cdr matrix) (- from-row 1) column))))
  (define (clean-up matrix until-row column)
    (if (zero? until-row)
        matrix
        (cons (map -
                   (car matrix)
                   (map (lambda (element)
                          (/ (* element (list-ref (car matrix) column))
                             (list-ref (list-ref matrix until-row) column)))
                        (list-ref matrix until-row)))
              (clean-up (cdr matrix) (- until-row 1) column))))
  (define (normalise matrix row with-column)
    (if (zero? row)
        (cons (map (lambda (element)
                     (/ element (list-ref (car matrix) with-column)))
                   (car matrix))
              (cdr matrix))
        (cons (car matrix) (normalise (cdr matrix) (- row 1) with-column))))
  (define (repeat procedure matrix indices)
    (if (null? indices)
        matrix
        (repeat procedure
                (procedure matrix (car indices) (car indices))
                (cdr indices))))
  (define (iota start stop)
    (if (> start stop)
        (list)
        (cons start (iota (+ start 1) stop))))
  (let ((indices (iota 0 (- (length matrix) 1))))
    (repeat normalise
            (repeat clean-up
                    (repeat clean-down
                            matrix
                            indices)
                    indices)
            indices)))
;; ---

(def data
  (list->vector
   (for/collect (line (in-file-lines "day24"))
     (map string->number (pregexp-split "[ ,@]+" line)))))

(definline (cross vx vy wx wy)
  (- (* vx wy) (* vy wx)))

(def (part1)
  (def p1 0)
  (for* ((i (in-range 0 (vector-length data)))
         (j (in-range (1+ i) (vector-length data))))
    (with (([px py _ rx ry _] (vector-ref data i))
           ([qx qy _ sx sy _] (vector-ref data j)))
      (let (rxs (cross rx ry sx sy))
        (unless (zero? rxs)
          (let ((t (/ (cross (- qx px) (- qy py) sx sy) (exact->inexact rxs)))
                (u (/ (cross (- qx px) (- qy py) rx ry) (exact->inexact rxs))))
            (when (and (<= 0 t)
                       (<= 0 u)
                       (<= 200000000000000 (+ px (* t rx)) 400000000000000)
                       (<= 200000000000000 (+ py (* t ry)) 400000000000000))
              (increment! p1)))))))
  p1)

(def (part2)
  (with (([p1x p1y p1z v1x v1y v1z] (vector-ref data 0))
         ([p2x p2y p2z v2x v2y v2z] (vector-ref data 1))
         ([p3x p3y p3z v3x v3y v3z] (vector-ref data 2)))

    (def A [[(- (- v1y v2y)) (- v1x v2x) 0 (- p1y p2y) (- (- p1x p2x)) 0]
            [(- (- v1y v3y)) (- v1x v3x) 0 (- p1y p3y) (- (- p1x p3x)) 0]
            [0 (- (- v1z v2z)) (- v1y v2y) 0 (- p1z p2z) (- (- p1y p2y))]
            [0 (- (- v1z v3z)) (- v1y v3y) 0 (- p1z p3z) (- (- p1y p3y))]
            [(- (- v1z v2z)) 0 (- v1x v2x) (- p1z p2z) 0 (- (- p1x p2x))]
            [(- (- v1z v3z)) 0 (- v1x v3x) (- p1z p3z) 0 (- (- p1x p3x))]])

    (def b [(- (- (* p1y v1x) (* p2y v2x)) (- (* p1x v1y) (* p2x v2y)))
            (- (- (* p1y v1x) (* p3y v3x)) (- (* p1x v1y) (* p3x v3y)))
            (- (- (* p1z v1y) (* p2z v2y)) (- (* p1y v1z) (* p2y v2z)))
            (- (- (* p1z v1y) (* p3z v3y)) (- (* p1y v1z) (* p3y v3z)))
            (- (- (* p1z v1x) (* p2z v2x)) (- (* p1x v1z) (* p2x v2z)))
            (- (- (* p1z v1x) (* p3z v3x)) (- (* p1x v1z) (* p3x v3z)))])

    (def augment 
      (map (lambda (A b) (append A (list b))) A b))

    (apply + (take (map last (reduced-row-echelon-form augment)) 3))))

(def (main . args)
  (displayln (part1))                   ; 20963
  (displayln (part2)))                  ; 999782576459892
