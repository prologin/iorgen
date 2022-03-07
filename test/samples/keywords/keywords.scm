;;; if_: not a condition
;;; class: not a class
;;; i: just a string
;;; in: not in
;;; for: not a loop
;;; words: contains lots of things
;;; words-1: an integer
(define (keywords if_ class i in for words words-1)
  ;;; TODO If this compiles, it is already a good step!
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

(let* ((if_ (string->number (read-line)))
       (class (string-ref (read-line) 0))
       (i (read-line))
       (in (map cons '(a static) (parse-int-list (read-line))))
       (for (parse-int-list (read-line)))
       (words
         (make-list
           2
           (lambda
             ()
             (make-assoc-list
               '(int if-true)
               (list
                 (lambda
                   ()
                   (make-assoc-list
                     '(return void)
                     (list
                       (lambda () (string->number (read-line)))
                       (lambda () (parse-int-list (read-line))))))
                 (lambda () (string->number (read-line))))))))
       (words-1 (string->number (read-line))))
  (keywords if_ class i in for words words-1))
