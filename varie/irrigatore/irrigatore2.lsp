#|
prog {a, b, c, d}, frequenza { giorni {1-31}, dow {l,ma,me,g,v,s,d}, odd/even}, orario, stazione {1-4}, durata

prog {a, b, c, d}, giorno (di irrigazione) {ogni {1-31}, odd {0,1}, even {0,1}, dow {l,ma,me,g,v,s,d}}, orario, stazione {1-4}, durata

prog giorno (di irrigazione) tipo parametri
prog orari partenza 
prog stazione durata

(giorno prog tipo ...)
(orario prog ora)
(durata prog stazione minuti)

|#


(def LocalDate &java.time.LocalDate)

(def giorni ())
(def\ (giorno (#: (or 'a 'b 'c 'd) prog) . param)
  (match param
    ( ((#: (or 'every 'odd 'even 'o/e 'dow) tipo) . param)
      (set! giorni
        (insertBefore (\ (a b) (> (car a) (car b)))
          (list* prog tipo
            (case tipo
              (every (def ((#: (>= 1) n)) param) (cons n (@now LocalDate)))
              (odd (def (#: () b) param) b)
              (even (def (#: () b) param) b)
              (o/e (def ((#: (or 0 1) b)) param) (list b))
              (dow (def (#: (7 (or 0 1)) b) param) (list->array b)) ))
          (remove [_ (== (car _) prog)] giorni) )))
    ( ()
      (set! giorni (remove [_ (== (car _) prog)] giorni)) )
    (else
      (log "invalid parameters:" (cons prog param)) ))
  giorni )
 
(giorno 'a 'every 1)
(giorno 'b 'even) 
(giorno 'c 'dow 1 1 1 1 1 1 1) 

(def LocalTime &java.time.LocalTime)

(def orari ())
(def orario
  (let*\
    ( ((removeOra ora) 
         (remove [_ (eq? (car _) ora)] orari) )
      ((removeProg prog ora)
         (set! orari
           (ifnull? (progs (ifnull? (progs (assoc ora orari :cmp eq?)) () (remove [_ (== _ prog)] (cdr progs)) ))
             (removeOra ora)
             (insertBefore (\ (a b) (> (car a) (car b)))
               (cons ora (remove [_ (== _ prog)] progs))
               (removeOra ora) )))) )
      (\ ((#: (or 'a 'b 'c 'd) prog) . param)
        (match param
          ( ((#: String ora))
            (def progs (ifnull? (progs (assoc ora orari :cmp eq?)) () (cdr progs)))
            (set! orari
              (insertBefore (\ (a b) (> (car a) (car b)))
                (cons ora (if (null? progs) (cons prog) (member? prog progs :cmp eq?) progs (sort (cons prog progs))))
                (removeOra ora) )))
          ( ((#: String ora) #f)
            (removeProg prog ora))
          ( ()
            (forEach (\ (ora) (removeProg prog ora)) (map car orari)) )
          (else
            (log "invalid parametes:" param) ))
        orari )))

(orario 'a "09:30")
(orario 'b "09:30")
(orario 'c "10:30")
(orario 'a "11:30")
;(orario 'a)
;(orario 'c "10:30" #f)

(def durate ())
(def\ (durata (#: (or 'a 'b 'c 'd) prog) . param)
  (def stazioni (ifnull? (stazioni (assoc prog durate)) () (cdr stazioni)))
  (match param
    ( ((#: (or 1 2 3 4) stazione) (#: (and Integer (>= 0)) minuti))
      (set! durate
        (insertBefore (\ (a b) (> (car a) (car b)))
          (cons prog (insertBefore (\ (a b) (> (car a) (car b))) (cons stazione minuti) (remove [_ (== (car _) stazione)] stazioni)))
          (remove [_ (== (car _) prog)] durate) )))
    ( ((#: (or 1 2 3 4) stazione))
      (set! durate
        (ifnull? (stazioni (remove [_ (== (car _) stazione)] stazioni))
          (remove [_ (== (car _) prog)] durate)
          (insertBefore (\ (a b) (> (car a) (car b)))
            (cons prog stazioni)
            (remove [_ (== (car _) prog)] durate) ))))
    ( () 
      (set! durate (remove [_ (== (car _) prog)] durate)) )
    (else
      (log "invalid parameter:" param)))
  durate )

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
  (def progs (filter [_ (today? (cdr (assoc _ giorni)))] (map car giorni)))
  (let1 loop (starts (filter [_ (>= _ now)] (map car orari)))
    (ifnull? ((start . starts) starts) ()
      (append
        (cons
          start
          (let1 loop (progs (intersect (assoc start orari :cmp eq?) progs))
            (ifnull? ((prog . progs) progs) ()
              (append (assoc prog durate) (loop progs)) )))
        (loop starts) ))))

(today "08:00") 

#|
(def Thread &java.lang.Thread)
(def TimeUnit &java.util.concurrent.TimeUnit)

(@sleep Thread 1000)
(@sleep (.SECONDS TimeUnit) 2)
(@sleep (.MINUTES TimeUnit) 2)


(def thread
  (new Thread
    (runnable
      (log "partito")
      (catchWth (caseType\ (exc) ((Error @getCause &java.lang.InterruptedException) (log "interrotto")) (else (throw exc)))
        (if test (@sleep (.SECONDS TimeUnit) 15) (exec tasks))
        (log "finito") ))))
(begin (@start thread) #inert)
(begin (@interrupt thread) #inert)

(if (@isAlive tread) (then (@interrupt thread) #inert))

(def\ (ore ora) (@parse LocalTime ora))
|#

(def Thread &java.lang.Thread)
(def TimeUnit &java.util.concurrent.TimeUnit)
(def LocalTime &java.time.LocalTime)
(def ChronoUnit &java.time.temporal.ChronoUnit)
(def DateTimeFormatter &java.time.format.DateTimeFormatter)
(def MINUTES (.MINUTES ChronoUnit))
(def SECONDS (.SECONDS ChronoUnit))
(def tasks ("09:30" a (1 . 6) (2 . 4) b (3 . 5) (4 . 2) "10:30" c (1 . 1) "11:30" a (1 . 6) (2 . 4)))
(def test #t)
(def\ (exec tasks . now)
  (def now (optDft now (@now LocalTime)))
  (ifnull? ((first . rest) tasks) 
    (let1 (minuti (1+ (@between MINUTES now (@parse LocalTime "23:59"))))
      (log "delay" ($ minuti "'") "fino alle" (@format (@plus now minuti MINUTES) (@ofPattern DateTimeFormatter "HH:mm")) "di domani")
      ;(@sleep (.MINUTES TimeUnit) minuti)
      "finito" )
    (then
      (match first
        ( (#: String ora)
          (def ora (@parse LocalTime ora))
          (def minuti (1+ (@between MINUTES now ora)))
          (when (> minuti 0l)
            (log "delay" ($ minuti "'"))
            (set! now
            	(if test (@plus now minuti MINUTES) 
                (else 
                  (@sleep (.MINUTES TimeUnit) minuti)
                  (@now LocalTime) ))))
          (log (@format now (@ofPattern DateTimeFormatter "HH:mm")) "start") )
        ( (#: Symbol prog)
          (log "  program" prog) )
        ( ((#: Integer stazione) . (#: Integer minuti))
          (log "    station" stazione ($ minuti "'"))
          (set! now
            (if test (@plus now minuti MINUTES)
            (else
              (@sleep (.MINUTES TimeUnit) minuti)
              (@now LocalTime) )))) )
      (apply** exec rest (if test (cons now) ())) )))
(exec tasks (@parse LocalTime "08:30"))

(def test #f)
(def tasks ("16:38" a (1 . 1) (2 . 1) b (3 . 1) (4 . 1) "16:44" c (1 . 1) "16:47" a (1 . 1) (2 . 1)))
(exec tasks)



