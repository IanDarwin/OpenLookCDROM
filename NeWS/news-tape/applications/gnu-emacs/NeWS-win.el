;; NeWS interface for GNU Emacs.
;;
;; Author:	Chris Maio
;; Last edit:	4 Sep 1988

(defvar NeWS-max-selection-size 1024
  "*Maximum size text selection we will make available to NeWS clients.
Large values will crash clients not prepared to handle large blocks of text.")

;; A-list containing the valid NeWS options.
(setq command-switch-alist
      (append '(("-origin" . NeWS-option-origin)
		("-xy" . NeWS-option-origin)
		("-dimensions" . NeWS-option-dimensions)
		("-size" . NeWS-option-dimensions)
		("-drag" . NeWS-option-drag)
		("-reshape" . NeWS-option-reshape)
		("-framelabel" . NeWS-option-framelabel)
		("-fl" . NeWS-option-framelabel))
	      command-switch-alist))

(defun NeWS-option-arg (type)
  (let ((result (car command-line-args-left)))
    (setq command-line-args-left (cdr command-line-args-left))
    (cond ((eq type 'string) result)
	  ((eq type 'int) (string-to-int result)))))

(defun NeWS-option-origin (ignored)
  (NeWS-set-origin (NeWS-option-arg 'int) (NeWS-option-arg 'int)))

(defun NeWS-option-dimensions (rest)
  (NeWS-set-dimensions (NeWS-option-arg 'int) (NeWS-option-arg 'int)))

(defun NeWS-option-framelabel (rest)
  (NeWS-set-framelabel (NeWS-option-arg 'string)))

(defun NeWS-set-i&d-line-ok (bool)
  "Specify whether it's ok to use hardware insert- and delete-line functions.
Disable this if you don't like what happens in partially-obscured windows.
See \"NeWS-set-scroll-region-ok.\""
  (interactive (list (y-or-n-p "Use insert- and delete-line functions? ")))
  (NeWS-set-option 1 bool))

(defun NeWS-set-i&d-char-ok (bool)
  "Specify whether it's ok to use rasterops to move characters on the display.
This function doesn't really work."
  (interactive (list (y-or-n-p "Use insert- and delete-char functions? ")))
  (NeWS-set-option 2 bool))

(defun NeWS-set-scroll-region-ok (bool)
  "Specify whether it's ok to use the display hardware's scrolling function.
Hardware scrolling looks better but currently causes odd behavior in
partially-obscured windows.  See \"NeWS-set-i&d-line-ok.\""
  (interactive (list (y-or-n-p "Use scroll regions? ")))
  (NeWS-set-option 3 bool))

(defun NeWS-set-window-retained (retain)
  "Specify whether or not the NeWS window should be retained.
Retained windows behave better in some circumstances, but worse in others;
among other things, scrolling and line-insert/delete functions may be
noticably slower in obscured, retained windows."
  (interactive (list (y-or-n-p "Retain this window? ")))
  (NeWS-set-option 4 retain))

(defun NeWS-set-input-policy (stuff)
  "If ARG is non-nil, \"stuff\" text rather than using insert-string.
When copying \"selections\" into the Emacs NeWS window, \"stuffing\" text
simulates typein, while insertion is similar to yanking text."
  (interactive
   (list
    (y-or-n-p "NeWS Selections should \"Stuff\" rather than \"Insert\"? ")))
  (NeWS-set-option 5 stuff))

;;; Do not ask the user to draw out the screen bounds
(defun NeWS-option-drag (&optional ignored)
  (NeWS-set-option 6 nil))

;;; Ask the user to draw out screen bounds with mouse at startup
(defun NeWS-option-reshape (&optional ignored)
  (NeWS-set-option 6 t))

;; Input event handling
;;
;; Note: I have no idea what the differences between Primary, Secondary,
;; and Shelf Selections are, so Emacs current only supports one selection.

(setq NeWS-b-marker (make-marker)
      NeWS-z-marker (make-marker))

;; NeWS input handler indicates event info available with C-x NUL
(define-key global-map "\C-x\C-@" 'NeWS-handle-event)

;; NeWS-next-event returns one of the following:
;;
;;	(0 string)			; execute string
;;	(1 string)			; insert string
;;	(2 rank size x y)		; set-selection-at
;;	(3 rank size x y)		; extend-selection-to
;;
;; This is just a rough cut at provide simple copy/paste functions.
;; I've never used SunView so I have no idea what this "selection service"
;; stuff is all about.  Briefly, SetSelectionAt (normally the left mouse
;; button) starts a new selection, and ExtendSelectionTo (middle button)
;; grows it or shrinks it, setting the "mark" appropriately.  The NeWS
;; function keys can then be used to copy the selection to the "shelf" and
;; paste it into another window.
;; 
;; You can also use (NeWS-set-selection 0 1 "your string here") to make
;; arbitrary strings available to the NeWS server.

(defun NeWS-handle-event nil
  "Handle a NeWS input event.  Don't call this function."
  (interactive)
  (let ((elist (NeWS-next-event)))
    (if (> 2 (car elist))
	(cond
	 ((eq (car elist) 0)		; string insertion request
	  (push-mark) (insert-string (nth 1 elist)))
	 ((eq (car elist) 1)		; command execution request
	  (eval (car (read-from-string (concat "(" (nth 1 elist) ")"))))))
      (let (window (x (nth 3 elist)) (y (nth 4 elist)))
	(if (setq window (NeWS-coordinates-in-modeline-p x y))
	    ;; Mouse-down in mode line--should call a per-buffer function?
	    (and (select-window window)
		 (if (eq (car elist) 3)
		     (scroll-down)
		   (scroll-up)))
	  ;; Mouse down in buffer body
	  (cond
	   ((eq (car elist) 2)		;set selection ...
	    (and (NeWS-move-to x y)
		 (set-marker NeWS-b-marker (point))
		 (set-marker NeWS-z-marker (point))
		 (< (point-min) (point-max))
		 (NeWS-set-selection (nth 1 elist) (nth 2 elist)
				     (char-to-string
				      (or (char-after (point))
					  (char-after (1- (point))))))))
	   ((eq (car elist) 3)		;extend selection ...
	    (let (b z point oldpoint)
	      (and (setq oldpoint (point))
		   (NeWS-move-to x y)
		   (setq point (point))
		   (eq (current-buffer) (marker-buffer NeWS-b-marker))
		   (setq b (marker-position NeWS-b-marker))
		   (setq z (or (marker-position NeWS-z-marker) b))
		   (cond
		    ((>= point z)	; lean forward
		     (set-marker NeWS-z-marker (setq z point)))
		    ((<= point b)	; bend over backward
		     (set-marker NeWS-b-marker (setq b point)))
		    (t			; suck it in
		     (if (> (- z point) (- point b))
			 (set-marker NeWS-b-marker (setq b point))
		       (set-marker NeWS-z-marker (setq z point)))))
		   (push-mark (if (eq point z) b z))
		   (or (<= (- z b) NeWS-max-selection-size)
		       (error
		"Selection (%d chars) is larger than %d character maximum"
			(- z b) NeWS-max-selection-size))
		   (message "Setting selection from %d to %d (%d chars)"
			    b z (- z b))
		   (save-excursion
		     (if (or (eq oldpoint b) (eq oldpoint z))
			 (NeWS-set-selection (nth 1 elist) (nth 2 elist)
					     (buffer-substring b z))
		       ; selection is being extended in a non-obvious way,
		       ; so blink the far end of the range
		       (exchange-point-and-mark)
		       (NeWS-set-selection (nth 1 elist) (nth 2 elist)
					   (buffer-substring b z))
		       (sit-for 1)
		       (exchange-point-and-mark))))))))))))

(defun NeWS-coordinates-in-modeline-p (x y)
  "Return the window whose modeline occupies screen COLUMN and ROW, or NIL."
  (let* ((windowxy (window-at x y))
	 (window (car windowxy))
	 (wy (nth 2 windowxy)))
    (and window
	 (eq (- (window-height window) 1) wy)
	 window)))

(defun NeWS-move-to (x y)
  "Move the cursor to X Y in the appropriate window."
  (let ((windowlist (window-at x y)))
    (and windowlist
	 (select-window (car windowlist))
	 (move-to-window-line (nth 2 windowlist))
	 (move-to-column (nth 1 windowlist)))))

(defun NeWS-put-region nil
  "Copy the current region the NeWS Shelf Selection."
  (interactive)
  (NeWS-set-selection 2 1 (buffer-substring (region-beginning) (region-end))))

(defun NeWS-put-text (s)
  "Copy STRING to the NeWS Shelf Selection."
  (NeWS-set-selection 2 1 s))

(defun NeWS-get-text nil
  "Paste the NeWS Shelf Selection into the current buffer at point."
  (interactive)
  (NeWS-send-PostScript "/ShelfSelection getselection /ContentsAscii get" t))

(if (eq window-system 'NeWS)
    (progn
      (setq suspend-hook
	    '(lambda nil
	       (error "You can't suspend Emacs in a NeWS window.")))
      (if (boundp 'window-system-version)
	  (setq window-setup-hook 'NeWS-map-window)
	(setq term-setup-hook 'NeWS-map-window))
      (run-hooks 'NeWS-setup-hook)))
