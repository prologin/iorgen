;;; n: a number, used as a size
;;; list_: a list of structs
(define (example n list_)
  ;;; TODO In a real life scenario, you will describe here what you want the
  ;;; end user to do with this generated code
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

(define (make-assoc-list-oneline k b)
  (let loop ((l (string->list (read-line))) (k k) (b b) (c '()))
    (let ((conv (lambda () (if (car b)
                             (string->number (list->string (reverse c)))
                             (car c)))))
      (cond
        ((null? l) (list (cons (car k) (conv))))
        ((char=? #\space (car l)) (cons (cons (car k) (conv))
                                        (loop (cdr l) (cdr k) (cdr b) '())))
        (else (loop (cdr l) k b (cons (car l) c)))))))

(let* ((n (string->number (read-line)))
       (list_
         (make-list
           n
           (lambda
             ()
             (make-assoc-list-oneline '(integer character) '(#t #f))))))
  (example n list_))
