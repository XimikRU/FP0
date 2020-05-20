; 1. Определить макрос, который возвращает свой вызов.

(defmacro self-call-macro () 
    '(quote (self-call-macro))
)

(print (self-call-macro))

; 2. Определите макрос (POP стек), который читает из стека верхний 
; элемент и меняет значение переменной стека

(defmacro custom-POP (стек)
  `(prog1
     (setq temp (car ,стек))
     (setq ,стек (cdr ,стек))))

(setq новый-стек '(0 1 2 3 4))

(print (custom-POP новый-стек))
(print (custom-POP новый-стек))
(print (custom-POP новый-стек))
(print (custom-POP новый-стек))
(print (custom-POP новый-стек))
(print (custom-POP новый-стек))

; 3. Определите лисповскую форму (IF условие p q) в виде макроса.

(defmacro new-if (условие p q)
  `(if ,условие ,p ,q)
)

(print (new-if (= 1 1) 'true 'false))
(print (new-if (= 1 2) 'true 'false))
(print (new-if (> 1 2) 'true 'false))
(print (new-if (< 1 2) 'true 'false))

; 4. Определите ввиде макроса форму (FIF тест отр нуль полож).

(defmacro FIF (тест отр нуль полож)
  `(cond
     ((> ,тест 0) ,полож)
     ((< ,тест 0) ,отр)
     (T ,нуль)))

(print (FIF 0 'отр 0 'полож))
(print (FIF 1 'отр 0 'полож))
(print (FIF -1 'отр 0 'полож))

