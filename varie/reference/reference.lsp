;; comment -> html

(let ()

(def milli (@currentTimeMillis System))

(load "varie/html/html.lsp" (theEnv)) 

(def PrintWriter &java.io.PrintWriter)
(def FileReader &java.io.FileReader)
(def BufferedReader &java.io.BufferedReader)

(def readLine (getMethod BufferedReader "readLine"))
(def startsWith (getMethod String "startsWith" String))
(def indexOf (getMethod String "indexOf" &int))
(def indexOfFrom (getMethod String "indexOf" &int &int))
(def charAt (getMethod String "charAt" &int))

(def\ (nm d)
  (let* ( (b1 (indexOf d #\ ))
          (b2 (let1 (l (indexOfFrom d #\  (1+ b1))) (if (-1? l) (length d) l))) )
    (subSeq d
      (let1 (b1 (1+ b1)) (if (== (charAt d b1) #\x28) (1+ b1) b1))
      (if (startsWith d "\x28;def*") (indexOf d #\x29) b2) )))

(def base "https://htmlpreview.github.io?https://github.com/GiacomoCau/Wat/blob/main/reference/reference")
;(def base "/reference/reference")

(def files '("lispx/src/boot.lispx" "lispx/src/condSys.lispx"))

(def chapters 0)

(close1 (w (@new PrintWriter "reference/reference.html"))
  (dlet ((htmlWriter w))
    (pr "<!DOCTYPE html>")
    (html
      (head
        (title (pr "Wat/Lispx Reference"))
        (meta (@ 'charset "UTF-8"))
        (style
          (pr "body { margin-left: 2%; margin-top: 2%;}")
          (pr "ul { list-style-type: none; }") ))
      (body
        (h2 (pr "Wat/Lispx Reference"))
        (ol (@ 'start 0)
          (li (a (@ 'href "https://github.com/GiacomoCau/Wat?tab=readme-ov-file") (pr "Wat")))
          (doList (file files)
            (close1 (fr (@new BufferedReader (@new FileReader file)))
              (log file)
              (for1 (l (readLine fr)) (! (null? l)) 
                (continue? (! (startsWith l "#|!")))
                ;(until? (>= chapters 2))
                (+= chapters 1)
                (def chapter (encode (subSeq l 4)))
                (li (a (@ 'href ($ base chapters ".html")) (pr chapter))) ))) )
        (br) )) ))

(log chapters 'chapters)

(def* (chapter# def# l) 0 0 #null)

(def\ (buttons)
  (br)
  (a (@ 'href ($ base (-1+ chapter#) ".html"))
    (input (@ 'type "button" 'value "<" 'disabled (== chapter# 1))) )
  (a (@ 'href ($ base ".html")) 
    (input (@ 'type "button" 'value "^")) )
  (a (@ 'href ($ base (1+ chapter#) ".html"))
    (input (@ 'type "button" 'value ">" 'disabled (== chapter# chapters))) ) )
    
(doList (file files)
  (close1 (r (@new BufferedReader (@new FileReader file)))
    (log file)
    (loop (set! l (readLine r)) (until? (|| (null? l) (startsWith l "#|!"))))
    (loop (until? (null? l))
      ;(until? (>= chapter# 2))
      (+= chapter# 1)
      (def chapter ($ chapter# ". " (subSeq l 4)))
      (log 'chapter chapter)
      (def chapter (encode chapter))
      (close1 (w (@new PrintWriter ($ "reference/reference" chapter# ".html") "utf-8")) 
        (dlet ( (htmlDeep 0) (htmlWriter w) )
          ;(log htmlDeep 0) 
          (pr "<!DOCTYPE html>")
          (html
            (head
              (title (pr chapter))
              (meta (@ 'charset "UTF-8"))
              (style
                (pr "body { margin-left: 2%; }")
                (pr "ul { list-style-type: none; }")
                (pr "a { text-decoration: none; }") ))
            (body
              (buttons)
              (h2 (pr chapter))
              (unless (startsWith (set! l :rhs (readLine r)) " |#")
                (ul (li (pr (encode (subSeq l 2))) (until (startsWith (set! l :rhs (readLine r)) " |#") (br (pr (encode (subSeq l 2))))))))
              (loop
                (set! l :rhs (readLine r)) 
                (continue?- 1 (|| (null? l) (startsWith l "#|!")) (buttons))
                (continue? (startsWith l ";;!") (ul (li (pr (encode (subSeq l 3))))))
                (continue? (startsWith l "  #|!") (ul (li (until (startsWith (set! l :rhs (readLine r)) "   |#") (br (pr (encode (subSeq l 4))))) )))
                (continue? (startsWith l "#|") (until (or (startsWith (set! l :rhs (readLine r)) "|#") (startsWith l "  |#")) ))
                (continue? (! (|| (startsWith l "\x28;def") (startsWith l "\x28;%def"))))
                (def l0 l)
                (loop
                  (+= def# 1)
                  (set! l0 l)
                  (while? (startsWith (set! l :rhs (readLine r)) "\x28;def"))
                  (div
                    (h3 (pr (encode (nm l0))))
                    (ul (li (pr (encode l0)))) ))
                (if (startsWith l "  #|")
                  (div
                    (h3 (pr (encode (def name :rhs (nm l0)))))
                    (ul
                      (li
                        (pr (encode (subSeq l 4)))
                        (loop 
                          (until? (startsWith (set! l :rhs (readLine r)) "   |#"))
                          (br (pr (encode ((\ (s) (if (startsWith s "\x28;fn") (@replace s "fn" name) s)) (subSeq l 4)) ))) ))))
                  (div
                    (h3 (pr (encode (nm l0))))
                    (ul (li (pr (encode l0)))) )))) )))) ))

(log chapter# 'chapters def# 'definitions)

(log (@formatted "Reference ended in %tM'%1$TS\"" (@new Date (- (@currentTimeMillis System) milli))))

#inert )
