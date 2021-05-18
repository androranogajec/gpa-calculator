(define (grade-point-converter grade)
  (cond
        ((equal? grade 'A)
         (word 4.0))
        ((equal? grade 'B)
         (word 3.0))
        ((equal? grade 'C)
         (word 2.0))
        ((equal? grade 'D)
         (word 1.0)) 
        (else
         (word 0.0))))

(define (grade-point-modifier grade)
  (cond
    ((member? '+ grade)
     (+ (grade-point-converter (first grade)) .33))
    ((member? '- grade)
     (- (grade-point-converter (first grade)) .33))
    (else
     (grade-point-converter grade))))
  
(define (grade-point-combiner grades)
(every grade-point-modifier grades))

(define (grade-point-count grades)
  (count (grade-point-combiner grades)))

(define (grade-point-sum grades)
  (accumulate + (grade-point-combiner grades)))

(define (gpa grades)
  (/ (grade-point-sum grades)
     (grade-point-count grades)))

(gpa '(A A+ B+ B))