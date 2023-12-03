(import :std/srfi/1
        :std/iter
        :std/text/basic-parsers
        :std/pregexp)
(export main)

(def (parse-game game)
  (def r 0)
  (def g 0)
  (def b 0)
  (for (color (pregexp-split ", " game))
    (match (pregexp-split " " color)
      ([amount color]
       (match [color (string->number amount)]
         (["red"   n] (set! r n))
         (["green" n] (set! g n))
         (["blue"  n] (set! b n))))))
  (list r g b))

(def (parse file)
  (call-with-input-file file
    (lambda (input)
      (for/collect (line (in-input-lines input))
        (match (pregexp-split ": " line)
          ([id games]
           (cons (string->number (second (pregexp-split " " id)))
                 (map parse-game (pregexp-split "; " games)))))))))

(def (part1 data)
  (for/fold (sum 0) ([id . games] data)
    (+ sum (if (any (lambda (game)
                      (any > game '(12 13 14)))
                    games)
             0
             id))))


(def (part2 data)
  (for/fold (sum 0) ([id . games] data)
    (let (power (apply * (apply map max games)))
      (+ sum power))))

(def (main . args)
  (let (data (parse "day02"))
    (displayln (part1 data))            ; 2076
    (displayln (part2 data))))          ; 70950
