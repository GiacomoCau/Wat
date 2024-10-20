
(load "varie/html/html.lsp")

(h2 (pr "1") (pr "2"))

(html (@ 'aa 1 'bb "2" 'cc #t) (body (br (pr 3))))

(close1 (pw (@new &java.io.PrintWriter "prova.html"))  
  (dlet1 (htmlWriter pw)
    (html (@ 'aa 1 'bb "2" 'cc #t)
      (head
        (title (pr "Wat/Lispx Reference"))
        (style (pr "ul { list-style-type: none; }")) ) 
      (body
        (input (@ 'type "button" 'value "<" 'disabled (== a 1)))
        (br (pr 3)) ))))

