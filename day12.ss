(import :std/srfi/1
        :std/iter
        :std/misc/ports
        :std/misc/hash
        :std/pregexp
        :std/sugar
        "./advent.ss")
(export main)

(def (parse)
  (for/collect (line (in-file-lines "day12"))
    (with ([pattern numbers] (pregexp-split " " line))
      [pattern (map string->number (pregexp-split "," numbers))])))

(def (lop str n)
  "Remove first n chars of string."
  (substring str n (string-length str)))

(def memo (hash))

(def (count pattern numbers)
  (hash-ensure-ref memo [pattern numbers]
    (lambda ()
      (cond
       ((and (null? numbers) (equal? "" pattern))
        1)
       ((equal? "" pattern)
        0)
       ((and (null? numbers) (string-prefix? "#" pattern))
        0)
       ((string-prefix? "." pattern)
        (count (lop pattern 1) numbers))
       ((< (string-length pattern) (fold + 0 numbers))
        0)
       ((string-prefix? "?" pattern)
        (+ (count (lop pattern 1) numbers)
           (count (string-append "#" (lop pattern 1)) numbers)))
       ((string-prefix? "#" pattern)
        (let* ((n (first numbers))
               (dot (or (string-index pattern #\.)
                        (string-length pattern)))
               (remaining (lop pattern n)))
          (cond
           ((null? numbers)
            0)
           ((< dot n)
            0)
           ((equal? "" remaining)
            (count remaining (cdr numbers)))
           ((string-prefix? "#" remaining)
            0)
           (else
            (count (lop remaining 1)
                   (cdr numbers))))))))))
            
(def (unfold o)
  (if (string? o)
    (string-join (make-list 5 o) "?")
    (concatenate (make-list 5 o))))

(def (main . args)
  (let (data (parse))
    (displayln (for/foldl (+ 0) ([pattern numbers] data)
      (count pattern numbers)))         ; 7771
    (displayln (for/foldl (+ 0) ([pattern numbers] data)
      (count (unfold pattern)
             (unfold numbers))))))      ; 10861030975833
    
