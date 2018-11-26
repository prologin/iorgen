;;; n: the first number
;;; other-number: the second number
(define (simple n other-number)
  ;;; TODO Just do what you want with these numbers, like sum them.
  (newline))

(let* ((n (string->number (read-line)))
       (other-number (string->number (read-line))))
  (simple n other-number))
