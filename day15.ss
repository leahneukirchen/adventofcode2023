(import :std/iter
        :std/misc/ports
        :std/sugar
        :std/pregexp
        "./advent.ss")
(export main)

(def (alist-update alist k v)
  (let loop ((alist alist))
    (if (null? alist)
      (list (cons k v))
      (if (equal? (caar alist) k)
        (cons (cons k v) (cdr alist))
        (cons (car alist) (loop (cdr alist)))))))

(def (alist-delete alist k)
  (let loop ((alist alist))
    (if (null? alist)
      alist
      (if (equal? (caar alist) k)
        (cdr alist)
        (cons (car alist) (loop (cdr alist)))))))

(defrule (update! (getf ...) fun)
  (set! (getf ...) (fun (getf ...))))

(def (hash-string str)
  (for/fold (h 0) (byte str)
    (remainder (* (+ h (char->integer byte)) 17) 256)))

(def (part1)
  (for/foldl (+ 0) (s (pregexp-split "," (read-file-string "day15")))
    (hash-string s)))

(def (part2)
  (let (boxes (make-vector 256 '()))
    (for (instr (pregexp-split "," (read-file-string "day15")))
      (match (pregexp-match "(.*)([=-])(.*)" instr)
        ([_ label "=" flen]
         (update! (vector-ref boxes (hash-string label))
                  (cut alist-update <> label (string->number flen))))
        ([_ label "-" _]
         (update! (vector-ref boxes (hash-string label))
                  (cut alist-delete <> label)))))

    (for/foldl (+ 0) ((boxx boxes)
                      (i (in-naturals)))
      (for/foldl (+ 0) (([_ . flen] boxx)
                        (slot (in-naturals)))
        (* (1+ i) (1+ slot) flen)))))

(def (main . args)
  (displayln (part1))                   ; 514639
  (displayln (part2)))                  ; 279470
