(import :std/srfi/1
        :std/iter
        :std/misc/ports
        :std/pregexp
        "./advent.ss")
(export main)

(define (scan kons knil xs)
  (reverse (fold (lambda (elt acc)
                   (cons (kons elt (car acc)) acc))
                 (list knil)
                 xs)))

(def data
  (for/collect (line (in-file-lines "day18"))
    (with ([dir n color] (pregexp-split "[ (#)]+" line))
      [(string->symbol dir)
       (string->number n)
       (string->number (substring color 0 5) 16)
       (string-ref color 5)]
      )))

(def plan 
  (map (match <>
         (['R n _ _] [n 0])
         (['D n _ _] [0 n])
         (['L n _ _] [(- n) 0])
         (['U n _ _] [0 (- n)]))
       data))

(def plan2
  (map (match <>
         ([_ _ n #\0] [n 0])
         ([_ _ n #\1] [0 n])
         ([_ _ n #\2] [(- n) 0])
         ([_ _ n #\3] [0 (- n)]))
       data))

(def (corners plan)
  (scan (lambda (a e) (map + a e)) [0 0] plan))

(def (shoelace corners)
  (fold (match <...>
          ([[ax ay] [bx by] sum]
           (+ sum (* 1/2 (- (* ax by) (* ay bx))))))
        0
        corners
        (cdr corners)))

(def (main . args)
  (displayln (+ (shoelace (corners plan))
                (* 1/2 (fold + 0 (map second data))) 1))  ; 45159
  (displayln (+ (shoelace (corners plan2))
                (* 1/2 (fold + 0 (map third data))) 1)))  ; 134549294799713
