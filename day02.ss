(import :std/srfi/1
        :std/iter
        :std/pregexp
        "./advent.ss")
(export main)

(def (parse-game game)
  (def r 0)
  (def g 0)
  (def b 0)
  (for (color (pregexp-split ", " game))
    (with ([amount color] (pregexp-split " " color))
      (match [color (string->number amount)]
        (["red"   n] (set! r n))
        (["green" n] (set! g n))
        (["blue"  n] (set! b n)))))
  (list r g b))

(def (parse file)
  (for/collect (line (in-file-lines file))
    (with ([id games] (pregexp-split ": " line))
      (cons (string->number (second (pregexp-split " " id)))
            (map parse-game (pregexp-split "; " games))))))

(def (part1 data)
  (for/foldl (+ 0) ([id . games] data)
    (if (any (lambda (game)
               (any > game '(12 13 14)))
             games)
      0
      id)))


(def (part2 data)
  (for/foldl (+ 0) ([id . games] data)
    (apply * (apply map max games))))

(def (main . args)
  (let (data (parse "day02"))
    (displayln (part1 data))            ; 2076
    (displayln (part2 data))))          ; 70950
