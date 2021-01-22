;;; n: the first list's size
;;; list-int: a list containing ints
;;; size: an other size
;;; list-char: a list of char
;;; string: a string
;;; list-string4: a list of strings of size 4
;;; list-list-string2: a list of list of strings of size 2 of size 2 of size 2
;;; matrix: a matrix of int
(define (lists n list-int size list-char string list-string4 list-list-string2 matrix)
  ;;; TODO Aren't these lists beautifull?
  (newline))

(define (parse-int-list str)
  (if (string=? "" str) '()
    (let loop ((l (string->list str)) (s 1) (i 0))
      (cond
        ((null? l) (list (* i s)))
        ((char=? #\space (car l)) (cons (* i s) (loop (cdr l ) 1 0)))
        ((char=? #\- (car l)) (loop (cdr l) (* -1 s) i))
        (else (loop (cdr l) s (+ (* i 10) (- (char->integer (car l)) 48))))))))

(define (make-list i f) (if (= 0 i) '() (cons (f) (make-list (- i 1) f))))

(let* ((n (string->number (read-line)))
       (list-int (parse-int-list (read-line)))
       (size (string->number (read-line)))
       (list-char (string->list (read-line)))
       (string (read-line))
       (list-string4 (make-list size read-line))
       (list-list-string2 (make-list 2 (lambda () (make-list 2 read-line))))
       (matrix (make-list size (lambda () (parse-int-list (read-line))))))
  (lists n list-int size list-char string list-string4 list-list-string2 matrix))
