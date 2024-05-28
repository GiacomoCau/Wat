(ddef* (htmlDeep htmlWriter) 0 (.out System))

(def\ (pr . args)
  (@print (htmlWriter) (@repeat "  " (htmlDeep)))
  (forEach# [_ (@print (htmlWriter) _)] args) 
  (@println (htmlWriter)) )

(def encode
  (let1 (encode (@getMethod Utility "encode" String))
    (\ (str) (encode #null str)) ))

(def\ (tag name . ac)
  (def name (list 'quote name))
  (def ac (optDft ac #f))
  (def\ (attr b forms) (if b (cdar forms) #null)) 
  (def\ (body b forms) (if b (cdr forms) forms)) 
  (macro forms
    (def attr? (&& (cons? forms) (cons? (car forms)) (== (caar forms) 'attr)))
    (list 'atEnd
       (if ac #inert (list 'endTag name))
       (list* 'startTag ac name (attr attr? forms))
       (list 'dlet1 '(htmlDeep (1+ (htmlDeep)))
         (cons 'begin (body attr? forms))
         #inert ))))

#| TODO da definire altrove
(def\ (join sep l) ((rec\ (f l) (if (null? l) "" ($ sep (car l) (f (cdr l))))) l))
(def\ (join sep l) ((rec\ (f l) (if (null? l) "" (let1 (cdr (cdr l)) (if (null? cdr) ($ (car l)) ($ (car l) sep (f cdr)))))) l))
|#

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
(def meta (tag 'meta #t))
