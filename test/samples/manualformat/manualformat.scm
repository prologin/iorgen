;;; a: a first number
;;; b: a second number
;;; c: a third number
;;; n: This one on a new line
;;; one-per-line: an integer list, one per line
(define (manual-format a b c n one-per-line)
  ;;; TODO From the function perspective, this is just 4 integers
  (newline))

(define (make-list i f) (if (= 0 i) '() (cons (f) (make-list (- i 1) f))))

(let* ((a (let ((i (read))) (begin (read-char) i)))
       (b (let ((i (read))) (begin (read-char) i)))
       (c (string->number (read-line)))
       (n (string->number (read-line)))
       (one-per-line (make-list 3 (lambda () (string->number (read-line))))))
  (manual-format a b c n one-per-line))
