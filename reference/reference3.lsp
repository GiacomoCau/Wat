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
    `(begin (startTag ,ac ',name ,@(attr at forms)) (++ htmlDeep) ,@(body at forms) (-- htmlDeep) ,@(if `,ac '(#inert) `((endTag ',name) #inert)) ) ))

(def\ (startTag ac name . attr)
  (pr "<" name (join " " attr) (if ac "/" "") ">") )

(def\ (endTag name)
  (pr "</" name ">") )

;(def\ (join sep l) ((rec\ (f l) (if (null? l) "" ($ sep (car l) (f (cdr l))))) l))
;(def\ (join sep l) ((rec\ (f l) (if (null? l) "" (let1 (cdr (cdr l)) (if (null? cdr) ($ (car l)) ($ (car l) sep (f cdr)))))) l))
(def\ (join sep lst)
  (def\ (attr lst) 
    (if (null? lst) ""
      (let1 ((key value . lst) lst)
        (if (== key value) ($ " " value)
          ($ sep key "=" (if (type? value String) ($ "\"" (encode value) "\"") value) (attr lst)) ))))
  (attr lst) )

(def html (tag 'html))
(def head (tag 'head))
(def title (tag 'title))
(def style (tag 'style))
(def body (tag 'body))
(def h2 (tag 'h2))
(def h3 (tag 'h3))
(def ul (tag 'ul))
(def li (tag 'li))
(def div (tag 'div))
(def br (tag 'br #t))

(def PrintWriter &java.io.PrintWriter)
(def FileReader &java.io.FileReader)
(def BufferedReader &java.io.BufferedReader)

(def\ (nm d)
  (let* ( (b1 (@indexOf d #\ ))
          (b2 (let1 (l (@indexOf d #\  (1+ b1))) (if (-1? l) (@length d) l))) )
    (@substring d (let1 (b1 (1+ b1)) (if (== (@charAt d b1) #\x28) (1+ b1) b1)) b2) ))       

(close
  ( (r (@new BufferedReader (@new FileReader "lispx/src/boot.lispx")))
    (w (@new PrintWriter "reference.html")) )
  (dlet ((htmlWriter w))
    (def n 0)
    (pr "<!DOCTYPE html>")
    (html
      (head
        (title (pr "Wat/Lispx Reference"))
        (style (pr "ul { list-style-type: none; }")) )
      (body  
        (for1 (l (@readLine r)) (!null? l)
          (cond  
            ( (@startsWith l ";;;")
                (h2 (pr (encode (@substring l 4)))) )
            ( (@startsWith l "\x28;def")
                (def l0 l)
                (loop
                  (+= n 1)
                  (set! l0 l)
                  (while! (@startsWith (set! l :rhs (@readLine r)) "\x28;def"))
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
                          (until! (@startsWith (set! l :rhs (@readLine r)) "   |$"))
                          (until! (@startsWith l "   |#")) ))
                      (if (@startsWith l "   |$")
                        (loop
                          (li (pr (encode (@substring l 4))))
                          (until! (@startsWith (def l :rhs (@readLine r)) "   |#")) )))) 
                  (div
                    (h3 (pr (encode (nm l0))))
                    (ul (li (pr (encode l0)))) )) )))))
    n ))

