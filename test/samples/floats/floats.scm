;;; f: a float
;;; g: a float, greater than f
;;; point: some coordinates
;;; n: a number
;;; float-list: a list of floats
;;; other-list: a list of floats
;;; inlined: some inlined structs
;;; multiline: a multiline struct
(define (floats f g point n float-list other-list inlined multiline)
  ;;; TODO Parsing is often easy, reprint mode is harder
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

(define (make-assoc-list-oneline k b)
  (let loop ((l (string->list (read-line))) (k k) (b b) (c '()))
    (let ((conv (lambda () (if (eq? 'char (car b))
                             (car c)
                             ((if (eq? 'int (car b)) values exact->inexact)
                              (string->number (list->string (reverse c))))))))
      (cond
        ((null? l) (list (cons (car k) (conv))))
        ((char=? #\space (car l)) (cons (cons (car k) (conv))
                                        (loop (cdr l) (cdr k) (cdr b) '())))
        (else (loop (cdr l) k b (cons (car l) c)))))))

(let* ((f (exact->inexact (string->number (read-line))))
       (g (exact->inexact (string->number (read-line))))
       (point (make-assoc-list-oneline '(x y z) '(float float float)))
       (n (string->number (read-line)))
       (float-list
         (map
           exact->inexact
           (read (open-input-string (string-append "(" (read-line) ")")))))
       (other-list
         (map
           exact->inexact
           (read (open-input-string (string-append "(" (read-line) ")")))))
       (inlined
         (make-list
           3
           (lambda
             ()
             (make-assoc-list-oneline
               '(integer char float)
               '(int char float)))))
       (multiline
         (make-assoc-list
           '(integer-2 string float-2)
           (list
             (lambda () (string->number (read-line)))
             (lambda () (read-line))
             (lambda () (exact->inexact (string->number (read-line))))))))
  (floats f g point n float-list other-list inlined multiline))
