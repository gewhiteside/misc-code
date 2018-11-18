(define current (lambda (listy) listy))

(define make-pairs
  (lambda (item listy)
    (map (lambda (elt)
           (list item elt))
         listy)))


(define make-all-pairs
  (lambda (listy list-func)
    (cond
      [(null? (list-func listy)) null]
      [else
       (append
        (make-pairs (first (list-func listy)) listy)
        (make-all-pairs listy (lambda (listy) (rest (list-func listy)))))])))


(make-all-pairs '(1 2 3) current)
