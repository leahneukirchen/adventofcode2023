(import :std/srfi/1
        :std/srfi/128
        :std/misc/hash
        :std/misc/queue
        :std/misc/number
        :std/misc/list-builder
        :std/misc/repr
        :std/iter
        :std/misc/ports
        :std/pregexp
        :std/sugar
        "./advent.ss")
(export main)

(def modules
  (list->hash-table
   (for/collect (line (in-file-lines "day20"))
     (with* (([_ broadcaster? type name targets] (pregexp-match "^(broadcaster|([%&])(\\w+)) -> (.*)" line))
             (targets (pregexp-split ", " targets)))
       (cons (or name "broadcaster")
             [type targets])))))

(def state (hash))

(def (reset-state!)
  (hash-clear! state)
  (for ((values name [type targets]) (in-hash modules))
    (for (target targets)
      (when (equal? (first (or (hash-get modules target) [#f []])) "&")
        (let (h (or (hash-get state target) (hash)))
          (hash-put! h name #f)
          (hash-put! state target h))))))

(def (handle state pulse)
  (with* (([src dst val] pulse)
          ([type targets] (or (hash-get modules dst) [#f []])))
    (match type
      ("%" (if (eq? val #f)
             (let (z (hash-get state dst))
               (hash-put! state dst (not z))
               (for/collect (t targets)
                 [dst t (not z)]))
             []))                       ; do nothing
      ("&" (let (h (or (hash-get state dst) (hash)))
             (hash-put! h src val)
             (hash-put! state dst h)
             (let (r (not (every (cut eq? #t <>) (hash-values h))))
               (for/collect (t targets)
                 [dst t r]))))
      (#f (for/collect (t targets)
            [dst t val])))))

(def (part1)
  (def lo 0)
  (def hi 0)

  (reset-state!)

  (let (q (make-queue))
    (for (_ (in-range 0 1000))
      (enqueue! q ["button" "broadcaster" #f])
    
      (until (queue-empty? q)
        (with ([src dst val] (dequeue! q))
          (match val
            (#f (increment! lo))
            (#t (increment! hi)))
          (let (new-pulses (handle state [src dst val]))
            (for (new-pulse new-pulses)
              (enqueue! q new-pulse)))))))
  (* lo hi))

(def (part2)
  (reset-state!)

  ;; specialized approach

  (def rx-input
    (let/esc return
      (for ((values name [type targets]) (in-hash modules))
        (when (member "rx" targets)
          (return name)))))

  (def dt-inputs
    (with-list-builder (push!)
      (for ((values name [type targets]) (in-hash modules))
        (when (member rx-input targets)
          (push! name)))))

  (def factors (hash))
  (def c 0)
  (while (< (hash-length factors) (length dt-inputs))
    (increment! c)
    (let (q (make-queue))
      (enqueue! q ["button" "broadcaster" #f])
      (until (queue-empty? q)
        (with ([src dst val] (dequeue! q))
          (let (new-pulses (handle state [src dst val]))
            (for (new-pulse new-pulses)
              (enqueue! q new-pulse)))
          (for (i dt-inputs)
          (unless (hash-key? factors i)
            (when (eq? #t (hash-ref (hash-ref state rx-input) i))
              (hash-put! factors i c))))))))
  (apply lcm (hash-values factors)))

(define (main . args)
  (displayln (part1))                   ; 711650489
  (displayln (part2)))                  ; 219388737656593
