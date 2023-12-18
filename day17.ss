(import :std/srfi/1
        :std/iter
        :std/misc/ports
        :std/pregexp
        :std/misc/pqueue
        :std/sugar)
(export main)

(def (digit-matrix file)
  (list->vector (map (lambda (line)
                       (list->vector
                       (map (lambda (c)
                              (- (char->integer c) (char->integer #\0)))
                            (string->list line))))
                     (read-file-lines file))))

(definline (matrix-ref matrix y x)
  (vector-ref (vector-ref matrix y) x))

(definline (matrix-pos? matrix y x)
  (and (< -1 y (vector-length matrix))
       (< -1 x (vector-length (vector-ref matrix y)))))

;; way too slow?
(def (find-end matrix min-travel max-travel end)
  (declare (not safe))
  (let ((queue (make-pqueue third <))
        (visited (hash)))
    (pqueue-push! queue [[0 0] [0 1] 0 0])
    (pqueue-push! queue [[0 0] [1 0] 0 0])
    (let/esc return
      (until (pqueue-empty? queue)
        (let (cur (pqueue-pop! queue))
          (unless (hash-key? visited cur)
            (with ([pos dir loss straight] cur)
              (hash-put! visited cur #t)
              (if (and (equal? pos end)
                       (>= straight (1- min-travel)))
                (return loss)
                (begin
                  (when (>= straight (1- min-travel))
                    ;; left
                    (with* ((new-dir (list (- (second dir))
                                           (first dir)))
                            ([ny nx] (map + pos new-dir)))
                      (when (matrix-pos? matrix ny nx)
                        (pqueue-push! queue [[ny nx] new-dir (+ loss (matrix-ref matrix ny nx)) 0])))
                    ;; right
                    (with* ((new-dir (list (second dir)
                                           (- (first dir))))
                            ([ny nx] (map + pos new-dir)))
                      (when (matrix-pos? matrix ny nx)
                        (pqueue-push! queue [[ny nx] new-dir (+ loss (matrix-ref matrix ny nx)) 0]))))
                  (when (< straight (1- max-travel))
                    (with (([ny nx] (map + pos dir)))
                      (when (matrix-pos? matrix ny nx)
                        (pqueue-push! queue [[ny nx] dir (+ loss (matrix-ref matrix ny nx)) (1+ straight)])))))))))))))
  
(def (main . args)
  (let* ((matrix (digit-matrix "day17"))
         (end [(1- (vector-length matrix))
               (1- (vector-length (vector-ref matrix 0)))]))
    (displayln (find-end matrix 1 3 end))     ; 886
    (displayln (find-end matrix 4 10 end))))  ; 1055
