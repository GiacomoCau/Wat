(def FileReader &java.io.FileReader)
(def BufferedReader &java.io.BufferedReader)
(def PrintWriter &java.io.PrintWriter)

(def encode
  (let1 (method (@getMethod Utility "encode" String))
    (\ (str) (method #null str)) ))

(def\ (nm d)
  (let* ( (b1 (@indexOf d #\ ))
          (b2 (let1 (l (@indexOf d #\  (1+ b1))) (if (-1? l) (@length d) l))) )
    (@substring d (let1 (b1 (1+ b1)) (if (== (@charAt d b1) #\x28) (1+ b1) b1)) b2) ))       

(close
  ( (r (@new BufferedReader (@new FileReader "lispx/src/boot.lispx")))
    (w (@new PrintWriter "reference.html")) )
  (def n 0)
  (def print (let1 (print print) (\ (l) #;(print l) (@println w l))))
  (print "<!DOCTYPE html>\n<html><head><title>Reference</title><style>ul { list-style-type: none; }</style></head>")
  (for1 (l (@readLine r)) (! (null? l))
      (if (@startsWith l ";;;") (then (print ($ "\n<h2>" (encode (@substring l 4)) "</h2>" )) (continue)))
      (if (! (@startsWith l "\x28;def")) (continue))
      (print "<div>")
      (def l0 l)
      (loop
        (+= n 1)
        (set! l0 l)
        (print ($ "<h3>" (encode (nm l0)) "</h3>"))
        (while? (@startsWith (set! l :rhs (@readLine r)) "\x28;def"))
        (print ($ "<ul><li>" (encode l0) "</ul></div>\n<div>")) )
      (if (@startsWith l "  #|")
        (then
          (print "<ul>\n<li>")
          (loop
            (set! l (@substring l 4))
            (print ($ (if (@startsWith l "$") "<li>" "") (encode l)))
            (until? (@startsWith (def l :rhs (@readLine r)) "   |#")) )
          (print "</ul>") )
        (print ($ "<ul><li>" (encode l0) "</ul></div>\n<div>")) )
      (print "</div>") )
   (print "</html>")
  n )
