;; comment -> html

(load "varie/html.lsp")

(def PrintWriter &java.io.PrintWriter)
(def FileReader &java.io.FileReader)
(def BufferedReader &java.io.BufferedReader)

(def\ (nm d)
  (let* ( (b1 (@indexOf d #\ ))
          (b2 (let1 (l (@indexOf d #\  (1+ b1))) (if (-1? l) (length d) l))) )
    (subSeq d (let1 (b1 (1+ b1)) (if (== (@charAt d b1) #\x28) (1+ b1) b1)) b2) ))       

(def base "https://htmlpreview.github.io?https://github.com/GiacomoCau/Wat/blob/main/reference/reference")
;(def base "/reference/reference")

(def chapters 0)
(close
  ( (r (@new BufferedReader (@new FileReader "lispx/src/boot.lispx")))
    (w (@new PrintWriter "reference/reference.html")) )
  (dlet ((htmlWriter w))
    (pr "<!DOCTYPE html>")
    (html
      (head
        (title (pr "Wat/Lispx Reference"))
        (meta (attr 'charset "UTF-8"))
        (style
          (pr "body { font-family: consolas; }")
          (pr "ul { list-style-type: none; }")) )
      (body
        (h2 (pr "Wat/Lispx Reference"))
        (ol 
          (for1 (l (@readLine r)) (! (null? l)) 
            (if (! (@startsWith l ";;;")) (continue))
            (+= chapters 1)
            (def chapter (encode (subSeq l 4)))
            (li (a (attr 'href ($ base chapters ".html")) (pr chapter))) ))))
    (log chapters 'chapters)
    chapters ))

(close1 (r (@new BufferedReader (@new FileReader "lispx/src/boot.lispx")))
  (def* (chapter# def# l) 0 0 #null)
  (def\ (buttons)
    (br)
    (a (attr 'href ($ base (1- chapter#) ".html"))
      (input (attr 'type "button" 'value "<" 'disabled (== chapter# 1))) )
    (a (attr 'href ($ base ".html")) 
      (input (attr 'type "button" 'value "^")) )
    (a (attr 'href ($ base (1+ chapter#) ".html"))
      (input (attr 'type "button" 'value ">" 'disabled (== chapter# chapters))) ) )
  (loop (set! l (@readLine r)) (until? (|| (null? l) (@startsWith l ";;;"))))
  (loop (until? (null? l))
    (+= chapter# 1)
    ;(until? (> chapter# 8))
    (def chapter ($ chapter# ". " (subSeq l 4)))
    (log 'chapter chapter)
    (def chapter (encode chapter))
    (close1 (w (@new PrintWriter ($ "reference/reference" chapter# ".html") "utf-8")) 
      (dlet ( (htmlDeep 0) (htmlWriter w) )
        (pr "<!DOCTYPE html>")
        (html
          (head
            (title (pr chapter))
            (meta (attr 'charset "UTF-8"))
            (style
              (pr "body { font-family: consolas; }")
              (pr "ul { list-style-type: none; }")
              (pr "a { text-decoration: none; }") ))
          (body
            (buttons)
            (h2 (pr chapter))
            (loop
              (set! l :rhs (@readLine r)) 
              (continue-? 1 (|| (null? l) (@startsWith l ";;;")) (buttons))
              (continue? (@startsWith l ";;!") (ul (li (pr (encode (subSeq l 3))))))
              (continue? (@startsWith l "#|!") (ul (until (@startsWith (set! l :rhs (@readLine r)) "|#") (li (pr (if (0? (length l)) "&nbsp;" (encode l)))))))
              (continue? (@startsWith l "#|") (until (@startsWith (set! l :rhs (@readLine r)) "|#")))
              (continue? (! (|| (@startsWith l "\x28;def") (@startsWith l "\x28;%def"))))
              (def l0 l)
              (loop
                (+= def# 1)
                (set! l0 l)
                (while? (@startsWith (set! l :rhs (@readLine r)) "\x28;def"))
                (div
                  (h3 (pr (encode (nm l0))))
                  (ul (li (pr (encode l0)))) ))
              (if (@startsWith l "  #|")
                (div
                  (h3 (pr (encode (nm l0))))
                  (ul
                    (li
                      (pr (encode (subSeq l 4)))
                      (loop 
                        ;(pr (encode (subSeq l 4)))
                        (until? (@startsWith (set! l :rhs (@readLine r)) "   |$"))
                        (until? (@startsWith l "   |#"))
                        (br (pr (encode (subSeq l 4)))) ))
                    (if (@startsWith l "   |$")
                      (loop
                        (li (pr (encode (subSeq l 4))))
                        (until? (@startsWith (def l :rhs (@readLine r)) "   |#")) )))) 
                (div
                  (h3 (pr (encode (nm l0))))
                  (ul (li (pr (encode l0)))) )))) ))))
    (list chapter# def#) )
