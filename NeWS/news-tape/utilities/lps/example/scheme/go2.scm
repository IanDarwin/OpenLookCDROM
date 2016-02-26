(lisp:eval-when (lisp:compile lisp:load lisp:eval)
		(excl:compile-file-if-needed "go2_h.scm")
		(load "go2_h"))
(excl:compile-file-if-needed "util.scm")
(load "util")

(define CROSS 0)
(define BLACK 1)
(define WHITE 2)

(define board (make-matrix BOARD_SIZE BOARD_SIZE))

(define (pickastone)
  (let ((i (random 7)))
    (if (> i 2) CROSS i)))

(if (not (ps_open_PostScript))
    (error "Cannot connect to NeWS server"))

(initialize)

(execute)

(let loop ()
  (if (not (psio_error PostScriptInput)) 
      (cond
       ((get_black)
	(let ((id get_black_id) (x get_black_x) (y get_black_y))
	  (if (= (matrix-ref board x y) BLACK)
	      (begin
	       (matrix-set! board x y CROSS)
	       (cross id x y))
	      (begin
	       (matrix-set! board x y BLACK)
	       (black_stone id x y)))
	  (loop)))
       ((get_white)
	(let ((id get_white_id) (x get_white_x) (y get_white_y))
	  (if (= (matrix-ref board x y) WHITE)
	      (begin
	       (matrix-set! board x y CROSS)
	       (cross id x y))
	      (begin
	       (matrix-set! board x y WHITE)
	       (white_stone id x y)))
	  (loop)))
       ((get_menu)
	(let loop1 ((x 0))
	  (if (< x BOARD_SIZE)
	      (let loop2 ((y 0))
		(if (< y BOARD_SIZE)
		    (begin
		     (matrix-set! board x y (if (= get_menu_cmd FILL_CMD)
						(pickastone)
						CROSS))
		     (loop2 (+ y 1)))
		    (loop1 (+ x 1))))))
	(repaint)
	(loop))
       ((get_damage)
	(draw_board get_damage_id)
	(let loop1 ((x 0))
	  (if (< x BOARD_SIZE)
	      (let loop2 ((y 0))
		(if (< y BOARD_SIZE)
		    (begin
		     (cond
		      ((= (matrix-ref board x y) BLACK)
		       (black_stone get_damage_id x y))
		      ((= (matrix-ref board x y) WHITE)
		       (white_stone get_damage_id x y)))
		     (loop2 (+ y 1)))
		    (loop1 (+ x 1))))))
	(repaired get_damage_id)
	(loop))
       ((not (or (done) (psio_eof PostScriptInput)))
	(loop)))))
(ps_close_PostScript)
