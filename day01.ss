(import :std/sugar
        :std/iter
        :std/pregexp
        "./advent.ss")
(export main)

(def (digit->number str)
  (- (char->integer (string-ref str 0))
     (char->integer #\0)))

(def english-digits
  (plist->hash-table
   '("one" "1"
     "two" "2"
     "three" "3"
     "four" "4"
     "five" "5"
     "six" "6"
     "seven" "7"
     "eight" "8"
     "nine" "9")))

(def (find-digit str :part part :pos pos)
  (let* ((rx (string-append
              (if (equal? pos 'last)
                ".*"
                "")
              "("
              (if (equal? part 1)
                "[0-9]"
                (string-join (cons "[0-9]" (hash-keys english-digits)) "|"))
              ")"))
         (match (cadr (pregexp-match rx str))))
    (digit->number (hash-ref english-digits match match))))

(def (solve part: part)
  (for/foldl (+ 0) (line (in-file-lines "day01"))
    (+ (* 10 (find-digit line part: part pos: 'first))
       (find-digit line part: part pos: 'last))))

(def (main . args)
  (displayln (solve part: 1))           ; 54644
  (displayln (solve part: 2)))          ; 53348
