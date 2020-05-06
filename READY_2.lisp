; 1. Определите FUNCALL через функционал APPLY.

(defun funcall-via-aplly (func &rest args) 
    (cond
        ((null args) NIL)
        (T (apply func args))
    )
)

(print(funcall-via-aplly `+ 1 2 3 4 5))

; 3. Определите функционал (APL-APPLY f x), который применяет 
; каждую функ-цию fi списка
; (f1 f2 ... fn)
; к соответствующему элементу списка
; x = (x1 x2 ... xn)
; и возвращает список, сформированный из результатов.

(defun APL-APPLY (funcs-list args-list)
    (cond 
        ((null args-list) NIL)
        ((null funcs-list) NIL) 
        (T (cons 
                (funcall (car funcs-list) (car args-list))
                (APL-APPLY (cdr funcs-list) (cdr args-list))
            )
        )
    )
)

(print(APL-APPLY `(car cdr) `((1 2 3 4 5) (6 7 8 9 0))))

; 5. Определите функциональный предикат (НЕКОТОРЫЙ пред список), который ис-тинен, 
; когда, являющейся функциональным аргументом предикат пред исти-нен хотя бы 
; для одного элемента списка список.

(defun НЕКОТОРЫЙ (пред список)
  (not (null (mapcan (lambda (x) (if (funcall пред x) (list T) NIL)) список))))

(print (НЕКОТОРЫЙ (lambda (x) (> x 5)) '(1 2 3 4 6)))
(print (НЕКОТОРЫЙ (lambda (x) (> x 5)) '(1 2 3 4 0)))
(print (НЕКОТОРЫЙ (lambda (x) (= x 5)) '(1 2 3 4 5)))
(print (НЕКОТОРЫЙ (lambda (x) (= x 5)) '(1 2 3 4 0)))

; 7. Определите фильтр (УДАЛИТЬ-ЕСЛИ-НЕ пред список), удаляющий из списка список
; все элементы, которые не обладают свойством, наличие которого проверяет
; предикат пред.

(defun УДАЛИТЬ-ЕСЛИ-НЕ (пред список)
    (mapcan (lambda (x) (if (funcall пред x) NIL (list x))) список))

(print (УДАЛИТЬ-ЕСЛИ-НЕ (lambda (x) (> x 5)) '(1 2 6 7 8 0)))
(print (УДАЛИТЬ-ЕСЛИ-НЕ (lambda (x) (= x 5)) '(1 5 2 4 5 0)))

; 11. Определите фукнционал МНОГОФУН, 
; который использует функции, являющиеся
; аргументами, по следующей схеме:
; (МНОГОФУН ’(f g ... h) x) , (LIST (f x) (g x) ... (h x))

(defun МНОГОФУН (funcs-list args-list)
    (mapcar (lambda (func) (apply func args-list)) funcs-list)
)

(print (МНОГОФУН '(+ - *) '(1 2 3 4 5)))