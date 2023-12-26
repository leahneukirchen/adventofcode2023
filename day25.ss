(import :std/srfi/1
        :std/srfi/113
        :std/srfi/128
        :std/iter
        :std/misc/hash
        :std/misc/list-builder
        :std/misc/number
        :std/misc/ports
        :std/misc/shuffle
        :std/sugar
        :std/pregexp
        "./advent.ss")
(export main)

(def (disjoint items)
  [(list->hash-table (map (lambda (x) (cons x x)) items))
   (list->hash-table (map (lambda (x) (cons x 0)) items))
   (length items)])

(def (ufind dj item)
  (let (parent (first dj))
    (unless (equal? (hash-ref parent item) item)
      (hash-put! parent item (ufind dj (hash-ref parent item))))
    (hash-ref parent item)))

(def (dunion dj item1 item2)
  (with ([parent rank count] dj)
    (let* ((item1 (ufind dj item1))
           (item2 (ufind dj item2)))
      (cond
       ((< (hash-ref rank item1) (hash-ref rank item2))
        (let (t item2)
          (set! item2 item1)
          (set! item1 t)))
       ((and (not (equal? item1 item2))
             (= (hash-ref rank item1) (hash-ref rank item2)))
        (increment! (hash-ref rank item1))))
      (hash-put! parent item2 item1)
      (unless (equal? item1 item2)
        (list-set! dj 2 (1- count))))))

(def (dsets dj)
  (let (roots (hash))
    (for (item (in-hash-keys (first dj)))
      (let (root (ufind dj item))
        (hash-put! roots root
                   (cons item (or (hash-get roots root) [])))))
    (hash-values roots)))

(def dcount third)

(def edges
  (with-list-builder (push!)
    (for (line (in-file-lines "day25"))
      (with ([src dsts] (pregexp-split ": " line))
        (for (dst (pregexp-split " " dsts))
          (push! (cons src dst))
          (push! (cons dst src)))))))

(def nodes (delete-duplicates (map car edges)))

(def (main . args)
  (let loop ()
    (def u (disjoint nodes))
    (let/esc break
      (for ([a . b] (shuffle edges))
        (unless (equal? (ufind u a) (ufind u b))
          (dunion u a b)
          (when (= 2 (dcount u))
            (break)))))
    (with ([a b] (dsets u))
      (if (= 3 (for/foldl (+ 0) ([src . dst] edges)
                 (if (and (member src a) (member dst b))
                   1
                   0)))
        (displayln (* (length a) (length b)))   ; 538368
        (loop)))))
  
