; cm2calentool - Convert from Calendar Manager to Calendar Tool
;                J.E.Black; GE Research 
;
; Version 1.0 - 05 Dec 1991
; Version 1.1 - 09 Dec 1991
; Version 1.2 - 24 Mar 1992
; Version 1.3 - 28 May 1992
; 
; calentool format
; YY MM DD HH MM TT [repeat-interval] <advance-warning> +times
; 
; cm format 
; (add "Fri Aug 16 14:00:00 1991" 
;  key: 1 
;  what: "Johnny's Birthday Party !\n"
;  details: "" 
;  duration: 10800 
   ; duration: 60 
   ; duration: 600 
   ; duration: 900 
   ; duration in seconds granularity slots in half hours.
   ; slots is number of EXTRA half hour slots needed.
;  period: weekly 
;  ntimes: 8 
;  exceptions: (0 2 4)            ; no longer present in openwin3
;  mailto: "emmett@rainbow"       ; no longer present in openwin3
;  author: "emmett@rainbow" 
;  attributes: (("ml","86400")    ; mail  time (removed in openwin3)
;               ("op","600")      ; open  time
;               ("fl","600")      ; flash time
;               ("bp","600")) )   ; beep  time
;
;  tags: ((toDo , 1)) 
;        ((toDo , 0))
;        ((appointment , 1)) 
;
;  apptstat: active 
;            completed
;  privacy: private 
;           public )
; 
; ========================================
(defconst key: 'key:)
(defconst what: 'what:)
(defconst details: 'details:)
(defconst duration: 'duration:)
(defconst period: 'period:)
(defconst ntimes: 'ntimes:)
(defconst exceptions: 'exceptions:)
(defconst mailto: 'mailto:)
(defconst author: 'author:)
(defconst attributes: 'attributes:)
(defconst tags: 'tags:)
(defconst apptstat: 'apptstat:)
(defconst privacy: 'privacy:)
;
; ========================================
(defconst single 'single)
(defconst daily  'daily)
(defconst weekly 'weekly)
(defconst biweekly 'biweekly)
(defconst monthly 'monthly)
; (defconst bimonthly 'bimonthly)
(defconst yearly  'yearly)
;
; ========================================
(defconst read   'read) 
(defconst write  'write)
(defconst delete 'delete)
(defconst exec   'exec)
;
; ========================================
(defconst toDo 'toDo)
(defconst appointment 'appointment)
(defconst active 'active)
(defconst completed 'completed)
(defconst private 'private)
(defconst public 'public)
;
; ========================================
(defmacro access (&rest args))
;
; ========================================
(defun insert-mailto ()
  (goto-char (point-min))
  (let ((bol 0)				; beginning of line
	(eol 0)				; end of line
	(spt 0)				; hold your spot
	(author "")			; mail name
	)
    (do ((lines 0 (1+ lines)))		; do it to every line
	((eq (point) (point-max)))	; until we reach the end
      (save-excursion (end-of-line)	; limit the searches
		      (setq eol (point)))
      (beginning-of-line)
      (setq bol (point))
      (when (re-search-forward "author: " eol t)
	(backward-char 10)
	(unless (looking-at "\"")
	  (forward-char 11)
	  (setq spt (point))
	  (re-search-forward "\"" eol t)
	  (backward-char 1)
	  (setq author (buffer-substring spt (point)))
	  (goto-char spt)
	  (backward-char 10)
	  (insert " mailto: \"" author "\"")
	  )
	)
      (forward-line 1))))
;
; ========================================
;
;x ;- insert missing attrbutes
;
;- change all [attributes: ((] 
;          to [attributes: '((]
;
;- change all [exceptions: (]
;          to [exceptions: '(]
;
;- change all [tags: ((]
;          to [tags: '((]
;
;- change all [("ml","]  to [("ml" "] ; mail
;             [("op","]  to [("op" "] 
;             ["(fl","]  to ["(fl" "] 
;             [("bp","]  to [("bp" "] 
;             [\n]       to [; ]      
;
(defun lispify () 
;  (goto-char (point-min))		; missing mailto
;  (insert-mailto)
;  (goto-char (point-min))		; missing mail time attribute
;  (replace-regexp "\(\(\"op\"" 
;		  "\(\(\"ml\",\"0\"\)\(\"op\"")
;  (goto-char (point-min))		; missing all attributes
;  (replace-regexp "\" \)" 
;		  "\" attributes: \(\(\"ml\",\"0\"\)\(\"op\",\"0\"\)\(\"fl\",\"0\"\)\(\"bp\",\"0\"\)\) \)")
  (goto-char (point-min))		; adjust attributes format
  (replace-regexp "attributes: ((" "attributes: \'((")
  (goto-char (point-min))
  (replace-regexp "tags: ((" "tags: \'((")
  (goto-char (point-min))
  (replace-regexp "(\"ml\",\"" "(\"ml\" \"")
  (goto-char (point-min))
  (replace-regexp "(\"op\",\"" "(\"op\" \"")
  (goto-char (point-min))
  (replace-regexp "(\"fl\",\"" "(\"fl\" \"")
  (goto-char (point-min))
  (replace-regexp "(\"bp\",\"" "(\"bp\" \"")
  (goto-char (point-min))		; adjust exceptions format
  (replace-regexp "exceptions: (" "exceptions: \'(")
  (goto-char (point-min))		; adjust new lines
  (replace-regexp "\\\\n" "; ")
  (goto-char (point-min)))
;
; ========================================
(defun dodate (datestring period)
  (let* ((dow   (substring datestring  0  3))
	 (month (substring datestring  4  7))
	 (days  (substring datestring  8 10))
	 (hour  (substring datestring 11 13))
	 (min   (substring datestring 14 16))
	 (year  (substring datestring 22 24))
	 (day   (string-to-int days))
	 (moy  '(("Jan" "01") ("Feb" "02") ("Mar" "03") 
		 ("Apr" "04") ("May" "05") ("Jun" "06") 
		 ("Jul" "07") ("Aug" "08") ("Sep" "09") 
		 ("Oct" "10") ("Nov" "11") ("Dec" "12")
		 ("**"  "**"))))
    (if (eq period 'yearly)  (setq year "**"))
    (if (eq period 'monthly) (setq month "**"))
    (format "%s %s %02d %s %s " 
	    year (nth 1 (assoc month moy)) day hour min)))
;
; ========================================
(defun interval (period)
  (case period
    (single   "")
    (daily    "[1] ")
    (weekly   "[7] ")
    (biweekly "[14] ")
    (monthly  "")			; MM = **
    (yearly   "")			; YY = **
    (t        "")))
;
; ========================================
(defun mailtime (attributes)
  (let* ((mst (nth 1 (assoc "ml" attributes)))
	 (mss (if (stringp mst) (string-to-int mst) 0))
	 (msd nil)
	 )
    (cond ((> mss 0)	
     (setq msd 
		   (if (> mss 86400)
		       (1+ (/ (1- mss) 86400)) 1))
	     (if (> msd 98) (setq msd 98))
	     (format "<%02d> " msd))
	  (t ""))))
;
; ========================================
(defun repcount (ntimes)
  (when (integerp ntimes)
    (if (> ntimes 0)
	(format "+%d " ntimes) "")))
;
; ========================================
(defun setpairs (string)
  (setq key nil) 
  (setq what nil)
  (setq details nil)
  (setq duration nil)
  (setq period nil)
  (setq ntimes nil)
  (setq exceptions nil)
  (setq mailto nil)
  (setq author nil)
  (setq attributes nil)
  (setq tags nil)
  (setq apptstat nil)
  (setq privacy nil)
  (let ((key nil) 
	(value nil)
	(args string))
    (do* ((key (car args) (car args))
	  (value (car (cdr args)) (car (cdr args)))
	  (args (cdr (cdr args)) (cdr (cdr args))))
	 ((null key))
      (case key
	(key: (setq key value))
	(what: (setq what value))
	(details: (setq details value))
	(duration: (setq duration value))
	(period: (setq period value))
	(ntimes: (setq ntimes value))
	(exceptions: (setq exceptions value))
	(mailto: (setq mailto value))
	(author: (setq author value))
	(attributes: (setq attributes value))
	(tags: (setq tags value))
	(apptstat: (setq apptstat value))
	(privacy: (setq privacy value))
	(t nil)))))
;
; ========================================
(defun add (date &rest arguments)
  (setpairs arguments)
  (let* ((slots (if duration 
		    (if (> duration 1800) (/ (1- duration) 1800) 0) 0))
	 (freq   (interval period))
	 (notice (mailtime attributes))
	 (repeat (repcount ntimes))
	 (class  (car (car tags)))
	 (ctbuffer *calentool*)
	 )
    (if (> slots 20) (setq slots 20))
    (if (eq class 'appointment)
	(save-excursion
	  (set-buffer ctbuffer)
	  (goto-char (point-max))
	  (newline)
	  (insert 
	   (format "%s%02d %s%s%s%s" 
		   (dodate date period) slots freq notice repeat what))))))
;
; ========================================
(defun remove (date &rest arguments))
; (remove "Mon May 27 07:42:00 1991" key: 40)
;
; ========================================
(defun cm2ct ()
  (interactive)
  (let* ((ctbuffer (get-buffer-create "*CalenTool*"))
	 (cmbuffer (current-buffer)))
    (setq *calentool* ctbuffer)
    (save-excursion
      (set-buffer cmbuffer)
      (goto-char (point-min))
      (lispify)
      (goto-char (point-min))
      (if (looking-at "Version") (insert "; "))
      (forward-line)
      (if (looking-at "****") (insert "; "))
      (forward-line)
      (if (looking-at "") (insert "; "))
      (forward-line)
      (unless (looking-at "(") (error "Unexpected ugly file format.")))
    (save-excursion
      (set-buffer ctbuffer)
      (goto-char (point-min))
      (insert "# CalenTool V2 - DO NOT REMOVE THIS LINE")
      (newline)(insert "# ")
      (newline)(insert "# This file generated from Calendar Manager file ")
      (newline)(insert "# on " (current-time-string))
      (newline)(insert "# by cm2ct version 1.3 of 28 May 1992 ")
      (newline)(insert "#    -- J. Emmett Black (blackje@crd.ge.com) ")
      (newline)(insert "# "))
    (set-buffer cmbuffer)
    (goto-char (point-min))
    (eval-current-buffer)
    ))
; 
