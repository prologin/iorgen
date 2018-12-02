;;; struct: a struct 1 instance
;;; n: a number
;;; struct-list: a list a struct 1
;;; triangle: a triangle
;;; struct-chars: a struct of chars
(define (structs struct n struct-list triangle struct-chars)
  ;;; TODO Look at them structs.
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

(let* ((struct (map cons '(foo bar) (parse-int-list (read-line))))
       (n (string->number (read-line)))
       (struct-list
         (make-list
           n
           (lambda () (map cons '(foo bar) (parse-int-list (read-line))))))
       (triangle
         (make-list
           3
           (lambda
             ()
             (make-assoc-list
               '(name pos)
               (list
                 (lambda () (string-ref (read-line) 0))
                 (lambda
                   ()
                   (map cons '(x y z) (parse-int-list (read-line)))))))))
       (struct-chars
         (map
           cons
           '(first-char second-char third-char)
           (let
             loop
             ((l (string->list (read-line))) (b #t))
             (if
               (null? l)
               '()
               (if b (cons (car l) (loop (cdr l) #f)) (loop (cdr l) #t)))))))
  (structs struct n struct-list triangle struct-chars))
