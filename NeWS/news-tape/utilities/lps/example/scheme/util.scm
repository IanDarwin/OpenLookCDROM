(define (make-matrix x y)
  (let ((v (make-vector x)))
  (let loop ((i 0))
    (if (< i x)
	(begin
	 (vector-set! v i (make-vector y 0))
	 (loop (+ i 1)))
	v))))

(define (matrix-ref m i j)
  (vector-ref (vector-ref m i) j))

(define (matrix-set! m i j v)
  (vector-set! (vector-ref m i) j v))

(define (rand-update x)
  (define a 25173)
  (define b 13849)
  (define m 65536)
  (modulo (+ (* a x) b) m))

(define rand
  (let ((x 1))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (random max)
  (modulo (rand) max))

(define (display-line . args)
  (for-each display args)
  (newline))
