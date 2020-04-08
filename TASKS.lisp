; Задачи 6, 15, 17, 18, 21, 22, 33, 42, 46, 47

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Задача 15
; Определите функцию, вычисляющую скалярное произведение векторов, заданных
; списками целых чисел.

(defun scalar (lst1 lst2)
    (if (or (null lst1)(null lst2)) 0
        (apply '+ (mapcar '* lst1 lst2))
    )
)

; Тесты
(print (scalar '(1 1) '(2 2)))
(print (scalar '(-5 -10) '(5 10)))
(print (scalar NIL NIL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Задача 18
; Определите предикат, проверяющий, является ли аргумент одноуровневым списком.

(defun is-list-single-level(lst)
    (cond
        ((null lst) T)
        ((listp (car lst)) NIL)
        (T (is-list-single-level (cdr lst)))
    )
)

; Тесты
(print (is-list-single-level '(()()())    ))
(print (is-list-single-level '()          ))
(print (is-list-single-level '(1 2 3 4)   ))
(print (is-list-single-level '(1 2 3 (4)) ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Задача 21
; Определите функцию, удаляющую из списка первое вхождение данного элемента на
; верхнем уровне.

(defun delete-element (lst element)
	(cond
		((null lst) nil)
		((= (car lst) element) (cdr lst))
		(T (cons (car lst) (delete-element (cdr lst) element)))
	)
)

; Тесты
(print (delete-element '(1 2 3 4 5) 5))
(print (delete-element '(1 5 1 5 1 2 3) 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Задача 22
; Определите функцию, которая обращает список (а b с) и разбивает его на уровни
; (((с) b) а)

(defun more-scopes(lst)
    (cond
        ((null (cdr lst)) lst)
        (T (list (more-scopes(cdr lst)) (car lst)))
    )
)

; Тесты
(print (more-scopes '(1 2 3 4 5 6 7 8 9 0) ))
(print (more-scopes '(1 2 3) ))
(print (more-scopes '() ))
