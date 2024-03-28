;; comment -> html

(ddef htmlDeep 0) 
(ddef htmlWriter (.out System))

(def\ (pr . args)
  (@print (htmlWriter) (@repeat "  " (htmlDeep)))
  (forEach# [_ (@print (htmlWriter) _)] args) 
  (@println (htmlWriter)) )

(def encode
  (let1 (method (@getMethod Utility "encode" String))
    (\ (str) (method #null str)) ))

(def\ (tag name . ac)
  (def ac (optDft ac #f))
  (def\ (attr b forms) (if b (cdar forms) #null)) 
  (def\ (body b forms) (if b (cdr forms) forms))
  (macro forms
    (def at (&& (cons? forms) (cons? (car forms)) (== (caar forms) 'attr)))
    `(begin
       (startTag ,ac ',name ,@(attr at forms))
       (+= htmlDeep 1)
       (atEnd
         ,(if `,ac '(-= htmlDeep 1) `(begin (-= htmlDeep 1) (endTag ',name)) )
         ,@(body at forms)
         ;; senza il seguente #inert (body (input)) andrebbe in errore (ma non (body (input 1)))
         ;; perch� comporrebbe (atEnd (-= htmlDeep 1)) che vuole almeno un'altra epressione
         #inert ))))

(def\ (join lst) 
  (if (null? lst) ""
    (let1 ((key value . lst) lst)
      (if (type? value Boolean) 
        (if value ($ " " key (join lst)) (join lst))
        ($ " " key "=" (if (type? value String) ($ "\"" (encode value) "\"") value) (join lst)) ))))

(def\ (startTag ac name . attr)
  (pr "<" name (join attr) (if ac "/" "") ">") )

(def\ (endTag name)
  (pr "</" name ">") )

#| TODO da definire altrove
(def\ (join sep l) ((rec\ (f l) (if (null? l) "" ($ sep (car l) (f (cdr l))))) l))
(def\ (join sep l) ((rec\ (f l) (if (null? l) "" (let1 (cdr (cdr l)) (if (null? cdr) ($ (car l)) ($ (car l) sep (f cdr)))))) l))
|#

(def html (tag 'html))
(def head (tag 'head))
(def title (tag 'title))
(def style (tag 'style))
(def body (tag 'body))
(def h1 (tag 'h1))
(def h2 (tag 'h2))
(def h3 (tag 'h3))
(def ul (tag 'ul))
(def ol (tag 'ol))
(def li (tag 'li))
(def div (tag 'div))
(def br (tag 'br #t))
(def a (tag 'a))
(def form (tag 'form))
(def input (tag 'input #t))

(def PrintWriter &java.io.PrintWriter)
(def FileReader &java.io.FileReader)
(def BufferedReader &java.io.BufferedReader)

(def\ (nm d)
  (let* ( (b1 (@indexOf d #\ ))
          (b2 (let1 (l (@indexOf d #\  (1+ b1))) (if (-1? l) (@length d) l))) )
    (@substring d (let1 (b1 (1+ b1)) (if (== (@charAt d b1) #\x28) (1+ b1) b1)) b2) ))       

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
        (style (pr "ul { list-style-type: none; }")) )
      (body
        (h2 (pr "Wat/Lispx Reference"))
        (ol 
          (for1 (l (@readLine r)) (!null? l)
            (if (! (@startsWith l ";;;")) (continue))
            (++ chapters)
            (def chapter (encode (@substring l 4)))
            (li (a (attr 'href ($ base chapters ".html")) (pr chapter))) ))))
    chapters ))

(close1 (r (@new BufferedReader (@new FileReader "lispx/src/boot.lispx")))
  (def* (chapter# def# l) 0 0 #null)
  (def\ (buttons)
    (a (attr 'href ($ base (1- chapter#) ".html"))
      (input (attr 'type "button" 'value "<" 'disabled (== chapter# 1))) )
    (a (attr 'href ($ base ".html")) 
      (input (attr 'type "button" 'value "^")) )
    (a (attr 'href ($ base (1+ chapter#) ".html"))
      (input (attr 'type "button" 'value ">" 'disabled (== chapter# chapters))) ) )
  (loop (set! l (@readLine r)) (until? (|| (null? l) (@startsWith l ";;;"))))
  (loop (until? (null? l))
    (+= chapter# 1)
    ;(until? (> chapter# 3))
    (print 'chapter chapter#)
    (def chapter (encode (@substring l 4)))
    (close1 (w (@new PrintWriter ($ "reference/reference" chapter# ".html") "utf-8")) 
      (dlet ( (htmlDeep 0) (htmlWriter w) )
        (pr "<!DOCTYPE html>")
        (html
          (head
            (title (pr chapter))
            (style
              (pr "ul { list-style-type: none; }")
              (pr "a { text-decoration: none; }") ))
          (body
            (buttons)
            (h2 (pr chapter))
            (loop
              (set! l :rhs (@readLine r)) 
              (continue-? 1 (|| (null? l) (@startsWith l ";;;")) (buttons))
              (continue? (! (@startsWith l "\x28;def")))
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
                      (loop 
                        (pr (encode (@substring l 4)))
                        (until? (@startsWith (set! l :rhs (@readLine r)) "   |$"))
                        (until? (@startsWith l "   |#")) ))
                    (if (@startsWith l "   |$")
                      (loop
                        (li (pr (encode (@substring l 4))))
                        (until? (@startsWith (def l :rhs (@readLine r)) "   |#")) )))) 
                (div
                  (h3 (pr (encode (nm l0))))
                  (ul (li (pr (encode l0)))) )))) ))))
    (list chapter# def#) )
