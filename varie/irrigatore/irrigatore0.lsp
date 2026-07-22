#|
programma {a, b, c, d}, frequenza { giorni {1-31}, dow {l,ma,me,g,v,s,d}, odd/even}, orario, stazione {1-4}, durata

programma {a, b, c, d}, giorno (di irrigazione) {ogni {1-31}, odd {0,1}, even {0,1}, dow {l,ma,me,g,v,s,d}}, orario, stazione {1-4}, durata

programma giorno (di irrigazione) tipo parametri
programma orari partenza 
programma stazione durata

(giorno programma tipo ...)
(orario programma ora)
(durata programma stazione minuti)

|#


(def LocalDate &java.time.LocalDate)

(def giorni (newObj))

(def\ (giorno (#: (or 'a 'b 'c 'd) programma) . param)
  (match param
    ( ((#: (or 'every 'odd 'even 'o/e 'dow) tipo) . param)
      (giorni programma
        (cons tipo
          (case tipo
            (every (def ((#: (>= 1) n)) param) (cons n (@now LocalDate)))
            (odd (def (#: () b) param) b)
            (even (def (#: () b) param) b)
            (o/e (def ((#: (or 0 1) b)) param) (list b))
            (dow (def (#: (7 (or 0 1)) b) param) (list->array b)) ))))
    ( ()
      (@remove giorni programma) )
    (else
      (log "parametri errati") ))
  giorni )

(giorno 'a 'every 1)
(giorno 'b 'even) 
(giorno 'c 'dow 1 1 1 1 1 1 1) 
giorni

(def LocalTime &java.time.LocalTime)
(def\ (ore ora) (@parse LocalTime ora))

(def orari (newObj))

(def\ (removeProg programma ora)
  (ifnull? (programmi (remove [_ (== _ programma)] (orari ora)))
    (@remove orari ora) 
    (orari ora programmi) ))

(def\ (orario (#: (or 'a 'b 'c 'd) programma) . param)
  (match param
    ( ((#: LocalTime ora))
      (def ora (@toString ora))
      (def programmi (value ora orari))
      (orari ora (if (null? programmi) (cons programma) (member? programma programmi) programmi (sort (cons programma programmi)))) )
    ( ((#: LocalTime ora) #f)
      (removeProg programma ora) )
    ( ()
      (forEach (\ (ora) (removeProg programma ora)) (@symbols orari)))
    (else
      (log "parametri errati") ))
  orari )
    
(orario 'a (ore "09:30"))
(orario 'b (ore "09:30"))
(orario 'c (ore "10:30"))
(orario 'a (ore "11:30"))
orari

;(def orari (newObj "" "09:30" '(a b) "10:30" '(c) "11:30" '(a) ))

(def durate (newObj))
(def\ (durata (#: (or 'a 'b 'c 'd) programma) . param)
  (def stazioni (value programma durate))
  (match param
    ( ((#: (or 1 2 3 4) stazione) (#: Integer minuti))
      (durate programma (append (if (null? stazioni) () (remove [_ (== (car _) stazione)] stazioni)) (cons (cons stazione minuti)))) )
    ( ((#: (or 1 2 3 4) stazione))
      (durate programma (remove [_ (== (car _) stazione)] stazioni)) )
    ( () 
      (@remove durate programma) )
    (else
      (log "parametri errati") ))
  durate )
;(def durate (newObj :a ((1 . 6) (2 . 4)) :b ((3 . 5) (4 . 2)) :c ((1 . 1))))
(durata 'a 1 6)
(durata 'a 2 4)
(durata 'b 3 5)
(durata 'b 4 2)
(durata 'c 1 1)

(def Calendar &java.util.Calendar)
(def ChronoUnit &java.time.temporal.ChronoUnit)
(def DAY_OF_MONTH (.DAY_OF_MONTH Calendar))
(def DAY_OF_WEEK (.DAY_OF_WEEK Calendar))

(def\ (today? (tipo . param))
  (case tipo
    (every (0? (% (@intValue (@between (.DAYS ChronoUnit) (@now LocalDate) (cdr param))) (car param))))
    (odd (1? (% (@get (@getInstance Calendar) DAY_OF_MONTH) 2)))
    (even (0? (% (@get (@getInstance Calendar) DAY_OF_MONTH) 2)))
    (o/e (== (car param) (% (@get (@getInstance Calendar) DAY_OF_MONTH) 2)))
    (dow (1? (arrayGet param (-1+ (@get (@getInstance Calendar) DAY_OF_WEEK))))) ))

(def\ (intersect a b)
  ((rec\ (loop a) (if (null? a) () (member? (car a) b)  (cons (car a) (loop (cdr a))) (loop (cdr a)))) a) )

(def\ (today . now)
  (def now (optDft now (@toString (@now LocalTime))))
  (def prgs (filter [_ (today? (giorni _))] (@symbols giorni)))
  (let1 loop (starts (filter [_ (>= _ now)] (@keys orari)))
    (ifnull? ((start . starts) starts) ()
      (append
        (cons
          start
          (let1 loop2 (prgs (intersect (orari start) prgs))
            (ifnull? ((prg . prgs) prgs) ()
              (append (list* prg (value prg durate)) (loop2 prgs)) )))
        (loop starts) ))))

(today "08:00") 

(@sleep &java.lang.Thread 1000)
(@sleep (.SECONDS &java.util.concurrent.TimeUnit) 2)
(@sleep (.MINUTES &java.util.concurrent.TimeUnit) 2)
