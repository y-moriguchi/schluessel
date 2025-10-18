(define testok 1)

(define-syntax tests1
  (syntax-rules ()
    ((_ a b) (list a b 1))))

(define 1+ (lambda (x) (+ x 1)))
(define 1- (lambda (x) (- x 1)))

; end of file
