;;; empty-list: an empty list
;;; buffer-string: here to check correct parsing of empty line above
;;; n: an integer, will be 0 in the sample input
;;; empty-in-sample: an empty list (only in the sample)
;;; empty-string: an empty string
;;; main: an other buffer string
;;; empty-char-list: an empty char list
;;; non-empty-char-list: an char list, non empty
;;; struct-with-empty-line: a struct containing an empty line, then a struct
;;; a-sized-struct: a sized struct containing an empty line
;;; finish: a string to finish
(define (empty-lines empty-list buffer-string n empty-in-sample empty-string main empty-char-list non-empty-char-list struct-with-empty-line a-sized-struct finish)
  ;;; TODO Wow, lots of empty lines!
  (newline))

(define (parse-int-list str)
  (if (string=? "" str) '()
    (let loop ((l (string->list str)) (s 1) (i 0))
      (cond
        ((null? l) (list (* i s)))
        ((char=? #\space (car l)) (cons (* i s) (loop (cdr l ) 1 0)))
        ((char=? #\- (car l)) (loop (cdr l) (* -1 s) i))
        (else (loop (cdr l) s (+ (* i 10) (- (char->integer (car l)) 48))))))))

(define (make-assoc-list k f)
  (if (null? k) '() (cons (cons (car k) ((car f)))
                          (make-assoc-list (cdr k) (cdr f)))))

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

(let* ((empty-list (parse-int-list (read-line)))
       (buffer-string (read-line))
       (n (string->number (read-line)))
       (empty-in-sample (parse-int-list (read-line)))
       (empty-string (read-line))
       (main (read-line))
       (empty-char-list (string->list (read-line)))
       (non-empty-char-list (string->list (read-line)))
       (struct-with-empty-line
         (make-assoc-list
           '(list-in-struct struct-in-struct)
           (list
             (lambda () (parse-int-list (read-line)))
             (lambda () (make-assoc-list-oneline '(char1 int2) '(#f #t))))))
       (a-sized-struct
         (let
           ((size (string->number (read-line))))
           (list (cons 'size size) (cons 'string-in-struct (read-line)))))
       (finish (read-line)))
  (empty-lines empty-list buffer-string n empty-in-sample empty-string main empty-char-list non-empty-char-list struct-with-empty-line a-sized-struct finish))
