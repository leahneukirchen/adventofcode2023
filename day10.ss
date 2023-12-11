(import :std/srfi/1
        :std/srfi/113
        :std/srfi/128
        :std/iter
        :std/misc/number
        :std/misc/list
        :std/misc/list-builder
        :std/misc/ports
        :std/pregexp
        :std/sugar)
(export main)

(def (char-matrix file)
  (list->vector (map (lambda (line) (string->vector line))
                     (read-file-lines file))))

(def (matrix-ref matrix y x)
  (vector-ref (vector-ref matrix y) x))

(def (find-S matrix)
  (let/esc return
    (for* ((y (in-range 0 (vector-length matrix)))
           (x (in-range 0 (vector-length (vector-ref matrix y)))))
      (when (equal? (matrix-ref matrix y x) #\S)
        (return [y x])))
    #f))

(def (step matrix pos dir) ; -> [new-pos new-dir]
  ;; do one step
  (alet* ((new-dir (match* ((matrix-ref matrix (first pos) (second pos)) dir)
                     ((#\| 'N) 'N)
                     ((#\| 'S) 'S)
                     ((#\- 'W) 'W)
                     ((#\- 'E) 'E)
                     ((#\L 'S) 'E)
                     ((#\L 'W) 'N)
                     ((#\J 'S) 'W)
                     ((#\J 'E) 'N)
                     ((#\7 'E) 'S)
                     ((#\7 'N) 'W)
                     ((#\F 'W) 'S)
                     ((#\F 'N) 'E)
                     ((_ _) #f)))
          (new-pos (match* (new-dir pos)
                     (('N [y x]) [(1- y) x])
                     (('S [y x]) [(1+ y) x])
                     (('W [y x]) [y (1- x)])
                     (('E [y x]) [y (1+ x)]))))
    [new-pos new-dir]))

(def (part1 matrix start)
  ;; XXX assume S is L, cba to try all cases here
  (vector-set! (vector-ref matrix (first start))
               (second start)
               #\L)

  ;; walk both nodes in parallel, until a common point is found
  (let loop ((n 0)
             (a (step matrix start 'S))
             (b (step matrix start 'W)))
    (if (equal? (first a) (first b))
      (1+ n)
      (loop (1+ n)
            (apply step matrix a)
            (apply step matrix b)))))

(def (part2 matrix start dir)
  (let ((p2 0)
        (path (list->set
               (make-default-comparator)
               (with-list-builder (push!)
                 (let loop ((next (step matrix start 'S)))
                   (push! (first next))
                   (unless (equal? (first next) start)
                     (loop (apply step matrix next))))))))
    (for ((row matrix)
          (y (in-naturals)))
      (let (in-loop? #f)
        (for ((_ row)
              (x (in-naturals)))
          (if (set-contains? path [y x])
            (case (matrix-ref matrix y x)
              ((#\| #\F #\7)
               (set! in-loop? (not in-loop?))))
            (when in-loop?
              (increment! p2))))))
    p2))

;; TODO use Pick's formula instead

(def (main . args)
  (let* ((matrix (char-matrix "day10"))
         (start (find-S matrix)))
    (displayln (part1 matrix start))           ; 6890
    (displayln (part2 matrix start 'S))))      ; 453
