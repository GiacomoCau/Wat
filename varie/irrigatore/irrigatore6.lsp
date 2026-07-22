
#|

programma {a, b, c, d}, giorno (di irrigazione) {every {1-31}, odd {0,1}, even {0,1}, o/e {0,1}, dow {l,ma,me,g,v,s,d}}, oraPartenza, stazione {1-4}, minnuti

giorno (di irrigazione) (howOften)
    prog tipo ...

orario (di partenza) (timeStart)
    prog ora
  
durata (di irrigazione) (runTime)
    prog stazione minuti

riduzione (in periodo umido)
	percentuale{0-100}

ritardo (per pioggia)
	giorni{0-31}

manuale
	prog stazione
|#

(def test #t)
(def maxProg 'd)
(def maxStaz 8)

(def\ (interruptThread)
  (if (&& (@isBound (theEnv) 'thread) (!null? thread) (@isAlive thread)) (then (@interrupt thread) #inert)))

(def LocalDate &java.time.LocalDate)

(def\ (==? b) (_ (== _ b)))
(def\ (car==? b) (_ (== (car _) b))) 
(def\ (>ab? a b) (> (car a) (car b)))

(def giorni ())
(def\ (giorno (#: (and Symbol (>= 'a) (<= maxProg)) prog) . param)
  (interruptThread)
  (match param
    ( ((#: (or 'every 'odd 'even 'o/e 'dow) tipo) . param)
      (set! giorni
        (insertBefore >ab?
          (list* prog tipo
            (case tipo
              (every (def ((#: (>= 1) n)) param) (cons n (@now LocalDate)))
              (odd (def (#: () b) param) b)
              (even (def (#: () b) param) b)
              (o/e (def ((#: (or 0 1) b)) param) (list b))
              (dow (def (#: (7 (or 0 1)) b) param) (list->array b)) ))
          (remove [car==? prog] giorni) )))
    ( ()
      (set! giorni (remove [car==? prog] giorni)) )
    (else
      (log "invalid parameters:" (cons prog param)) ))
  #;giorni )
 
(giorno 'a 'every 1)
(giorno 'b 'even) 
(giorno 'c 'dow 1 1 1 1 1 1 1) 
;(giorno 'c) ; elimina i giorni di irrigazione del programma c

(def LocalTime &java.time.LocalTime)
(def DateTimeFormatter &java.time.format.DateTimeFormatter)
(def HH:mm (@ofPattern DateTimeFormatter "HH:mm"))
(def\ (toTime s) (@parse LocalTime s HH:mm))

(def orari ())
(def orario
  (let*\
    ( ((removeOra ora) 
         (remove [_ (eq? (car _) ora)] orari) )
      ((removeProg prog ora)
         (set! orari
           (ifnull? (progs (ifnull? ((#_ . progs) (assoc ora orari :cmp eq?)) () (remove [==? prog] progs) ))
             (removeOra ora)
             (insertBefore >ab?
               (cons ora (remove [==? prog] progs))
               (removeOra ora) )))) )
      (\ ((#: (and Symbol (>= 'a) (<= maxProg)) prog) . param)
        (interruptThread)
        (match param
          ( ((#: (and String ((_ (toTime _) #t))) ora))
            (def progs (ifnull? ((#_ . progs) (assoc ora orari :cmp eq?)) () progs))
            (set! orari
              (insertBefore >ab?
                (cons ora (if (null? progs) (cons prog) (member? prog progs :cmp eq?) progs (sort (cons prog progs))))
                (removeOra ora) )))
          ( ((#: (and String ((_ (toTime _) #t))) ora) #f)
            (removeProg prog ora))
          ( ()
            (forEach [_ (removeProg prog _)] (map car orari)) )
          (else
            (log "invalid parameters:" param) ))
        #;orari )))
        
(orario 'a "09:30")
(orario 'b "09:30")
(orario 'c "10:30")
(orario 'a "11:30")
;(orario 'c "10:30" #f) ; elimina la partenza delle "10:30" del programma c
;(orario 'a) ; elimina tutte le partenze del programma a

(def durate ())
(def\ (durata (#: (and Symbol (>= 'a) (<= maxProg)) prog) . param)
  (interruptThread)
  (def stazioni (ifnull? ((#_ . stazioni) (assoc prog durate)) () stazioni))
  (match param
    ( ((#: (and Integer (>= 1) (<= maxStaz)) stazione) (#: (and Integer (>= 0)) minuti))
         (set! durate
           (insertBefore >ab?
             (cons prog (insertBefore >ab? (cons stazione minuti) (remove [car==? stazione] stazioni)))
             (remove [car==? prog] durate) )))
    ( ((#: (and Integer (>= 1) (<= maxStaz)) stazione))
         (set! durate
           (ifnull? (stazioni (remove [car==? stazione] stazioni))
             (remove [car==? prog] durate)
             (insertBefore >ab?
               (cons prog stazioni)
               (remove [car==? prog] durate) ))))
    ( () 
      (set! durate (remove [car==? prog] durate)) )
    (else
      (log "invalid parameters:" param)))
  #;durate )

(durata 'a 1 6)
(durata 'a 2 4)
(durata 'b 3 5)
(durata 'b 4 2)
(durata 'c 1 1)
;(durata 'c 1) ; elimina la durata di irrigazione della stazione 1 del programma c
;(durata 'c) ; elimina la durata di irrigazione di tutte le stazioni del programma c

(def ChronoUnit &java.time.temporal.ChronoUnit)
(def DAYS (.DAYS ChronoUnit))
(def Calendar &java.util.Calendar)
(def DAY_OF_MONTH (.DAY_OF_MONTH Calendar))
(def DAY_OF_WEEK (.DAY_OF_WEEK Calendar))

(def today 
  (let\ 
    ( ((today? (tipo . param))
         (let1 (calendar (@getInstance Calendar))
           (case tipo
             (every (0? (% (@intValue (@between DAYS (@now LocalDate) (cdr param))) (car param))))
             (odd (1? (% (@get calendar DAY_OF_MONTH) 2)))
             (even (0? (% (@get calendar DAY_OF_MONTH) 2)))
             (o/e (== (car param) (% (@get calendar DAY_OF_MONTH) 2)))
             (dow (1? (arrayGet param (-1+ (@get calendar DAY_OF_WEEK))))) )))
      ((intersect a b)
         ((rec\ (loop a) (if (null? a) () (member? (car a) b)  (cons (car a) (loop (cdr a))) (loop (cdr a)))) a) ))
    (\ now
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
            (loop starts) ))))))

;(today "08:00")
;(def task (today))

(def LocalTime &java.time.LocalTime)
(def MINUTES (.MINUTES ChronoUnit))
(def SECONDS (.SECONDS ChronoUnit))
#|
(def test #t)
(def tasks ("09:30" a (1 . 6) (2 . 4) b (3 . 5) (4 . 2) "10:30" c (1 . 1) "11:30" a (1 . 6) (2 . 4)))

(let1 (now (@now LocalTime)) (log now)  (@plusMinutes now (log (1+ (@between MINUTES now (@parse LocalTime "23:59:59"))))))
|#
(def\ (exec tasks . now)
  (def now (ifnull? (((#: (or String LocalTime) now)) now) (@now LocalTime) (if (type? now String) (toTime now) now)))
  (ifnull? ((first . rest) tasks) 
    (let1 (minuti (+ 2 (@between MINUTES now (toTime "23:59"))))
      (log "delay" ($ minuti "'") "fino alle" (@format (@plusMinutes now minuti) HH:mm) "di domani")
      ;(sleep minuti)
      ;(exec (today))
      "finito" )
    (else
      (match first
        ( (#: String ora)
          (def ora (toTime ora))
          (def minuti (+ 1 (@between MINUTES now ora)))
          (when (> minuti 0l)
            (log "delay" ($ minuti "'"))
            (sleep minuti) )
          (log (@format now HH:mm) "start") )
        ( (#: Symbol prog)
          (log "  program" prog) )
        ( ((#: (and Integer (>= 1) (<= maxStaz)) stazione) . (#: (and Integer (> 0)) minuti))
          (log "    station" stazione ($ minuti "'"))
          (open stazione minuti)) )
      (apply** exec rest (if test (cons now) ())) )))

(def TimeUnit &java.util.concurrent.TimeUnit)

(defde\ (sleep minuti)
  (set! now
  	(if test (@plusMinutes now minuti) 
      (else 
        (@sleep (.MINUTES TimeUnit) minuti)
        (@now LocalTime)))))

(defde\ (open stazione minuti)
  (sleep minuti) )
#|
(def test #t)
(def tasks ("09:30" a (1 . 6) (2 . 4) b (3 . 5) (4 . 2) "10:30" c (1 . 1) "11:30" a (1 . 6) (2 . 4)))
(exec tasks "08:30")

(def test #f)
(def tasks ("16:25" a (1 . 1) (2 . 1) b (3 . 1) (4 . 1) "16:31" c (1 . 1) "14:34" a (1 . 1) (2 . 1)))
(exec tasks)
|#

(def Thread &java.lang.Thread)
(def InterruptedException &java.lang.InterruptedException)

(def\ (startThread . param)
  (prog1
    (def thread :rhs
      (new Thread
        (runnable
          (log "partito")
          (catchWth (caseType\ (exc) ((Error @getCause InterruptedException) (log "interrotto")) (else (throw exc)))
            (apply exec (case (length param) (0 (cons (today))) ((1 2) param) (else (log "invalid parameters:" param))))
            (log "finito") ))))
    (@start thread)
    #inert ))
       
(def tasks ("09:30" a (1 . 6) (2 . 4) b (3 . 5) (4 . 2) "10:30" c (1 . 1) "11:30" a (1 . 6) (2 . 4)))     
(def thread (startThread tasks "08:30"))
;(def thread (startThread tasks))
;(def thread (startThread))