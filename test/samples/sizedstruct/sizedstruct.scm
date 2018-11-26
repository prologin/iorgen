;;; n: the size of the lists
;;; lists: a list of list of different sizes
;;; strings: a list of strings of different sizes
;;; matrices: a list of matrices of different sizes
;;; same: a list of list of same sizes
(define (sized-struct n lists strings matrices same)
  ;;; TODO The is a special case.
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

(define (make-assoc-list k f)
  (if (null? k) '() (cons (cons (car k) ((car f)))
                          (make-assoc-list (cdr k) (cdr f)))))

(let* ((n (string->number (read-line)))
       (lists
         (make-list
           n
           (lambda
             ()
             (let
               ((size1 (string->number (read-line))))
               (list
                 (cons 'size1 size1)
                 (cons 'int-list (parse-int-list (read-line))))))))
       (strings
         (make-list
           n
           (lambda
             ()
             (let
               ((size2 (string->number (read-line))))
               (list (cons 'size2 size2) (cons 'string-list (read-line)))))))
       (matrices
         (make-list
           2
           (lambda
             ()
             (let
               ((size3 (string->number (read-line))))
               (list
                 (cons 'size3 size3)
                 (cons
                   'list-list
                   (make-list
                     size3
                     (lambda () (parse-int-list (read-line))))))))))
       (same
         (make-list
           n
           (lambda
             ()
             (make-assoc-list
               '(size4 int-list-n)
               (list
                 (lambda () (string->number (read-line)))
                 (lambda () (parse-int-list (read-line)))))))))
  (sized-struct n lists strings matrices same))
