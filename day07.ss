(import :std/srfi/1
        :std/srfi/13
        :std/srfi/128
        :std/iter
        :std/misc/list
        :std/misc/ports
        :std/pregexp
        :std/sort
        :std/sugar)
(export main)

(def (type hand joker)
  (let* ((counts (chain (string-delete joker hand)
                   string->list
                   (sort <> char<?)
                   group-same
                   (map length <>)
                   (sort <> >)))
         (counts2 (if (null? counts)
                    (list (string-count hand joker))
                    (cons (+ (car counts)
                             (string-count hand joker))
                          (cdr counts)))))
    (match counts2
      ([5] 7)
      ([4 1] 6)
      ([3 2] 5)
      ([3 1 1] 4.5)
      ([2 2 1] 4)
      ([2 1 1 1] 3)
      ([1 1 1 1 1] 2))))

(def (weaker h1 h2 joker order)
  (comparator-if<=> (make-default-comparator) (type h1 joker) (type h2 joker)
    #t                    
    (<=? (make-default-comparator)
         (map (cut string-index order <>) (string->list h1))
         (map (cut string-index order <>) (string->list h2)))
    #f))
  
(def data
  (for/collect (line (read-file-lines "day07"))
    (match (pregexp-split " " line)
      ([hand bet] [hand (string->number bet)]))))

(def (solve data joker order)
  (apply + (map *
                (map second (sort data
                                  (lambda (l1 l2)
                                    (weaker (car l1) (car l2) joker order))))
                (iota (length data) 1))))

(def (main . args)
  (displayln (solve data #\X "23456789TJQKA"))     ; 251029473
  (displayln (solve data #\J "J23456789TQKA")))    ; 251003917
