(def FileReader &java.io.FileReader)
(def BufferedReader &java.io.BufferedReader)
(def PrintWriter &java.io.PrintWriter)

(close
  ( (r (@new BufferedReader (@new FileReader "lispx/src/boot.lispx")))
    (w (@new PrintWriter "reference.txt")) )
  (def n 0)
  (def print (let1 (print print) (\ (l) #;(print l) (@println w l))))
  (for (l (@readLine r)) (!null? l)
      (if (@startsWith l ";;;") (then (print ($ "\n" l)) (continue)))
      (if (! (@startsWith l "\x28;def")) (continue))
      (loop
        (+= n 1)
        (print (nm l))
        (while? (@startsWith (set! l :rhs (@readLine r)) "\x28;def")) )
      (if (@startsWith l "  #|")
        (loop
          (print ($ #\x9 (@substring l 4)))
          (until? (@startsWith (def l :rhs (@readLine r)) "   |#")
            ;(print l)
            ))))
  n)
