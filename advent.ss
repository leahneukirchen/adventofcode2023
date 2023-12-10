;; assorted helpers for Advent of Code

(import :std/contract
        :std/iter
        :std/misc/list-builder
        :std/pregexp
        :std/sugar)
(export #t)

(defrule (for/foldl (op init) (bind ...) body ...)
  (for/fold (acc init) (bind ...) (op (begin body ...) acc)))

(def (parse-list str)
  (with-input-from-string str
    (lambda ()
      (with-list-builder (push!)
        (let loop ((e (read)))
          (unless (eof-object? e)
            (push! e)
            (loop (read))))))))

(def (in-file-lines filename)
  (def (next it)
    (using (it :- iterator)
      (let (line (read-line it.e))
        (if (eof-object? line)
          iter-end
          line))))
  (def (fini it)
    (using (it :- iterator)
      (close-port it.e)))
  (make-iterator (open-input-file filename) next fini))

(define pregexp-replace*-with
  (lambda (pat str insf)
    ;return str with every occurrence of pat
    ;replaced by (insf match)
    (let ((pat (if (string? pat) (pregexp pat) pat))
          (n (string-length str)))
      (let loop ((i 0) (r ""))
        ;i = index in str to start replacing from
        ;r = already calculated prefix of answer
        (if (>= i n) r
            (let ((pp (pregexp-match-positions pat str i n)))
              (if (not pp)
                  (if (= i 0)
                      ;this implies pat didn't match str at
                      ;all, so let's return original str
                      str
                      ;else: all matches already found and
                      ;replaced in r, so let's just
                      ;append the rest of str
                      (string-append
                       r (substring str i n)))
                  (let* ((ins (insf (substring str (caar pp) (cdar pp))))
                         (ins-len (string-length ins)))
                    (loop (cdar pp)
                          (string-append
                           r
                           (substring str i (caar pp))
                           ins))))))))))
