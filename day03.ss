(import :std/srfi/1
        :std/iter
        :std/misc/number
        :std/misc/list
        :std/misc/ports
        :std/pregexp
        :std/sugar)
(export main)

;;; approach 1: a matrix

(def (digit->number ch)
  (- (char->integer ch)
     (char->integer #\0)))

(def (char-symbol? ch)
  (not (or (equal? ch #\.)
           (char-numeric? ch))))

(def (char-matrix file)
  (list->vector (map (lambda (line) (string->vector line))
                     (read-file-lines file))))

(def (matrix-ref matrix y x)
  (vector-ref (vector-ref matrix y) x))

(def (try-number matrix y x)
  (and (< -1 y (vector-length matrix))
       (< -1 x (vector-length (vector-ref matrix y)))
       (char-numeric? (matrix-ref matrix y x))
       (or (try-number matrix y (1- x))
           (let (n (digit->number (matrix-ref matrix y x)))
             (while (and (< x (1- (vector-length (vector-ref matrix y))))
                         (char-numeric? (matrix-ref matrix y (1+ x))))
               (increment! x)
               (set! n (+ (* 10 n) (digit->number (matrix-ref matrix y x)))))
             n))))

(def (part1 matrix)
  (let (part1 0)
    (for ((y (in-range 0 (vector-length matrix))))
      ;; we need do because we want to skip ahead
      (do ((x 0 (+ x 1)))
          ((>= x (vector-length (vector-ref matrix y))))

        (when (char-numeric? (matrix-ref matrix y x))
          (let* ((n (try-number matrix y x))
                 (len-n (string-length (number->string n))))
            (increment! x len-n)
            (when (let/esc return
                    (for ((sy (in-range (max (1- y) 0)
                                        (min (+ 2 y) (vector-length matrix)))))
                      (for ((sx (in-range (max (- x len-n 1) 0)
                                          (min (1+ x) (vector-length (vector-ref matrix sy))))))
                        (when (char-symbol? (matrix-ref matrix sy sx))
                          (return #t))))
                    #f)
              (increment! part1 n))))))
    part1))

(def (part2 matrix)
  (let (part2 0)
    (for ((y (in-range 0 (vector-length matrix))))
      (for ((x (in-range 0 (vector-length (vector-ref matrix y)))))

        (when (equal? (matrix-ref matrix y x) #\*)
          ;; this only works when no parts numbers are duplicated
          (let (found (delete-duplicates
                       (filter (lambda (x) x)
                               (concatenate
                                (for/collect ((ny (in-range (1- y) (+ 2 y))))
                                  (for/collect ((nx (in-range (1- x) (+ 2 x))))
                                    (try-number matrix ny nx)))))))
            (if (= (length found) 2)
              (set! part2 (+ part2 (apply * found))))
          ))))
    part2))

;;; approach 2: regexp, inspired by
;;; https://www.reddit.com/r/adventofcode/comments/189m3qw/2023_day_3_solutions/kbu2njq/

(def (pregexp-match-all-positions pat str)
  (let loop ((i 0)
             (matches '()))
    (if (>= i (string-length str))
      (reverse matches)
      (let (pos (pregexp-match-positions pat str i))
        (if pos
          (loop (cdar pos) (cons pos matches))
          (reverse matches))))))

(def (window3 xs)
  (zip xs (drop xs 1) (drop xs 2)))

(def (part1b)
  (let ((lines (append [""] (read-file-lines "day03") [""]))
        (part1 0))
    (for ((window (window3 lines)))
      (for (([[start . stop]]
             (pregexp-match-all-positions "\\d+" (second window))))
        (when (any (lambda (s)
                     (and (< start (string-length s))
                          (pregexp-match "[^0-9.]" s
                                         (max 0 (1- start)) 
                                         (min (string-length s) (1+ stop)))))
                   window)
          (increment! part1
                      (string->number (substring (second window) start stop))))))
    part1))

(def (part2b)
  (let ((lines (append [""] (read-file-lines "day03") [""]))
        (part2 0))
    (for ((window (window3 lines)))
      (for (([[start . stop]]
             (pregexp-match-all-positions "\\*" (second window))))
        (let (ns (with-list-builder (gather)
                   (for ((s window))
                     (for (([[a . b]] (pregexp-match-all-positions "\\d+" s)))
                       (when (or (<= (abs (- start a)) 1)
                                 (<= (abs (- stop b)) 1))
                         (gather (string->number (substring s a b))))))))
          (when (= (length ns) 2)
            (increment! part2 (apply * ns))))))
    part2))

(def (main . args)
  (let (matrix (char-matrix "day03"))
    (displayln (part1 matrix))                    ; 557705
    (displayln (part2 matrix))                    ; 84266818
    (displayln (part1b))
    (displayln (part2b))))
