; Задачи 6, 17, 33
; Сданы 15, 18, 21, 22, 42, 46, 47

; Задача 33
; Определите функцию МНОЖЕСТВО, преобразующую список в множество

(defun create-set(lst)
	((lambda(list1 list2)
		(
            cond((NULL lst) NIL)
                ((check list1 list2) (create-set list2))
                (T (cons list1 (create-set list2)))
		)
	)(car lst) (cdr lst))
)

(defun check(element lst)
	((lambda(list1 list2)
        (
            cond((NULL lst) NIL)
                ((eq element list1) T)
                (T (check element list2))
        )
    )(car lst)(cdr lst))
)

(print (create-set '(1 1 2 3 4 4 4 5 4 6 4 7)))
(print (create-set '(1 2 3 4 5 6 6 6 6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ПРИНЯТЫЕ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    ((lambda (f k)
        (cond
            ((null lst) nil)
            ((= f element) k)
            (T (cons f (delete-element k element)))
        )
    ) (car lst) (cdr lst))
)

; Тесты
(print (delete-element '(1 2 3 4 5) 5))
(print (delete-element '(1 5 1 5 1 2 3) 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Задача 22
; Определите функцию, которая обращает список (а b с) и разбивает его на уровни
; (((с) b) а)

(defun more-scopes(lst)
    ((lambda (f k)
        (cond
            ((null k) lst)
            (T (list (more-scopes k) f))
        )
    ) (car lst) (cdr lst))
)

; Тесты
(print (more-scopes '(1 2 3 4 5 6 7 8 9 0) ))
(print (more-scopes '(1 2 3) ))
(print (more-scopes '() ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Задача 42
; Определите функцию, находящую максимальное из значений, находящихся в
; вершинах дерева.

(defun max-in-tree (tree)
	(cond
		((null (cdr tree)) (car tree))
		(t (max (car tree) (max-in-tree (cadr tree)) (max-in-tree(caddr tree))))		
	)
)

; Тесты
(print (max-in-tree '(1 (2) (3))))
(print (max-in-tree '(1 (2 (3) (4)) (5))))
(print (max-in-tree '(1 (3 (5) (1)) (9 (10) (6)))))

; Задача 46. Предположим, что отец и мать некоторого лица, хранятся как  
; значения соответствующих свойств у символа, обозначающего это лицо. 
; Напишите функцию (РОДИТЕЛИ x), которая возвращает в качестве значения родителей,
; и предикат (СЕСТРЫ-БРАТЬЯ x1 x2), который истинен в случае, если x1 и x2 — сестры  
; или братья, родные или с одним общим родителем.

(defun link-parents(child mother father)
    (setf (get child 'mother) mother)
    (setf (get child 'father) father)
)

(defun get-parents(child)
    (list (get-mother child) (get-father child))
)

(defun get-father(child)
    (get child 'father)
)

(defun get-mother(child)
    (get child 'mother)
)

(defun is-sisters-or-brothers(child1 child2)
    (cond
        ((eq (get-mother child1) (get-mother child2)) T)
        ((eq (get-father child1) (get-father child2)) T)
        (T NIL)
    )
)

(link-parents 'CHILD_1 'MOTHER_1 'FATHER_1)
(link-parents 'CHILD_2 'MOTHER_2 'FATHER_2)
(link-parents 'CHILD_3 'MOTHER_2 'FATHER_1)
(link-parents 'CHILD_4 'MOTHER_3 'FATHER_3)

; Тесты
(print (is-sisters-or-brothers 'CHILD_1 'CHILD_2))
(print (is-sisters-or-brothers 'CHILD_2 'CHILD_3))
(print (is-sisters-or-brothers 'CHILD_3 'CHILD_1))
(print (is-sisters-or-brothers 'CHILD_2 'CHILD_4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Задача 47. Определите функцию УДАЛИТЬ-ВСЕ-СВОЙСТВА, которая удаляет все свойства символа.

(defun set-properties(X lst)
    (cond
        ; Если лист пустой, значит все присвоили
        ((null lst) T)
        (T
            ; Установим X[KEY(car lst)] = значение(cadr lst)
            (setf (get X (car lst)) (cadr lst))
            ; Идем по списку дальше с исключением 2 верхних элементов
            (set-properties X (cddr lst))      
        )
    )
)

(defun remove-properties(X)
    ((lambda(properties) 
        (cond
            ; Если список свойств пуст, значит все удалили
            ((null properties) T)
            (T
                ; Удалить свойство следующее из properties
                (remprop X (car properties))
                ; Идем дальше с модифицированным объектом
                (remove-properties X)        
            )
        )
    ; Эта функция возвращает список, который содержит пары свойств для X.
    ) (symbol-plist X))
)

(set-properties 'X '(a 1 b 2 c 3 d 4 e 5))
(print (symbol-plist 'X)) ; (E 5 D 4 C 3 B 2 A 1)  
(remove-properties 'X)
(print (symbol-plist 'X)) ; NIL
