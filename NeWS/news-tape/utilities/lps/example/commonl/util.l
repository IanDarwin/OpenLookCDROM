(defun vector-ref (v i)
  (svref v i))

(defun vector-set! (vec i val)
  (setf (svref vec i) val))

(defun make-vector (len val)
  (make-array (list len) :initial-element val))

(defun make-matrix (x y)
  (let ((v (make-vector x nil)))
    (do ((i 0 (+ i 1)))
	((not (< i x)) v)
	(vector-set! v i (make-vector y 0))
	)))

(defun matrix-ref (m i j)
  (vector-ref (vector-ref m i) j))

(defun matrix-set! (m i j v)
  (vector-set! (vector-ref m i) j v))

