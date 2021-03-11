'\"  Copyright 1992 Carnegie Mellon University and IBM.  All rights reserved.
'\" $Disclaimer: 
'\" Permission to use, copy, modify, and distribute this software and its 
'\" documentation for any purpose is hereby granted without fee, 
'\" provided that the above copyright notice appear in all copies and that 
'\" both that copyright notice, this permission notice, and the following 
'\" disclaimer appear in supporting documentation, and that the names of 
'\" IBM, Carnegie Mellon University, and other copyright holders, not be 
'\" used in advertising or publicity pertaining to distribution of the software 
'\" without specific, written prior permission.
'\" 
'\" IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
'\" DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
'\" ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
'\" SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
'\" BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
'\" DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
'\" WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
'\" ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
'\" OF THIS SOFTWARE.
'\"  $
.nr:a 0 1
.nr:b 0
.nr:f 0
.nr:g 0 1
.nr!N 0  
.dsBU o
.dsEM \%--
.dsF \u[\\n+(:p]\d
.dsRf \u[\\n+(:R]\d
.dsTm \uTM\d
.nrEc 0 1
.nrEx 0 1
.nrFg 0 1
.nrTb 0 1
.nrH1 0 1
.nrH2 0 1
.nrH3 0 1
.nrH4 0 1
.nrH5 0 1
.nrH6 0 1
.nrH7 0 1
.nrHu 2
.nrNp 0
.nrRf 0
'\"	-------------  FONTS  -----------------
.deB
.ie\\n(.$ .nr ;G \\n(.f
.el.ft 3
.if\\n(.$ .if !\\n(.$-2 \&\f3\\$1\fP\\$2
.if\\n(.$-2 \{.ds }i
.if~\\n(.f~2~ .ds }i \^
.ds}I \&\f3\\$1\fP\\$2\\*(}i
'br\}
.if\\n(.$-2 .if !\\n(.$-4 \\*(}I\f3\\$3\fP\\$4
.if\\n(.$-4 \\*(}I\f3\\$3\fP\\$4\\*(}i\f3\\$5\fP\\$6\\$7\\$8\\$9
.if\\n(.$ .ft \\n(;G
..
.deI
.ie\\n(.$ .nr ;G \\n(.f
.el.ft 2
.if\\n(.$ .if !\\n(.$-1 \&\f2\\$1
.if\\n(.$-1 \{.ds }i \^
.if~\\n(.f~2~ .ds }i
.ds}I \&
.if\w~\\$1~ .ds }I \&\f2\\$1\fP\\*(}i
'br\}
.if\\n(.$-1 .if !\\n(.$-3 \\*(}I\\$2\f2\\$3
.if\\n(.$-3 .if !\\n(.$-5 \\*(}I\\$2\f2\\$3\fP\\*(}i\\$4\f2\\$5
.if\\n(.$-5 \\*(}I\\$2\f2\\$3\fP\\*(}i\\$4\f2\\$5\fP\\*(}i\\$6\\$7\\$8\\$9
.if\\n(.$ .ft \\n(;G
..
.deRI
.nr;G \\n(.f
.}S 1 2 \& "\\$1" "\\$2" "\\$3" "\\$4" "\\$5" "\\$6\\$7\\$8\\$9"
..
.deRB
.nr;G \\n(.f
.}S 1 3 \& "\\$1" "\\$2" "\\$3" "\\$4" "\\$5" "\\$6\\$7\\$8\\$9"
..
.deIR
.nr;G \\n(.f
.}S 2 1 \& "\\$1" "\\$2" "\\$3" "\\$4" "\\$5" "\\$6\\$7\\$8\\$9"
..
.deIB
.nr;G \\n(.f
.}S 2 3 \& "\\$1" "\\$2" "\\$3" "\\$4" "\\$5" "\\$6\\$7\\$8\\$9"
..
.deBR
.nr;G \\n(.f
.}S 3 1 \& "\\$1" "\\$2" "\\$3" "\\$4" "\\$5" "\\$6\\$7\\$8\\$9"
..
.deBI
.nr;G \\n(.f
.}S 3 2 \& "\\$1" "\\$2" "\\$3" "\\$4" "\\$5" "\\$6\\$7\\$8\\$9"
..
.de}S
.ds}i
.if~\\$1~2~ .if !~\\$5~~ .ds }i\^
.ie!~\\$4~~ .}S \\$2 \\$1 "\\$3\f\\$1\\$4\\*(}i" "\\$5" "\\$6" "\\$7" "\\$8" "\\$9"
.el\\$3
.ft\\n(;G
..
'\"	ROMAN
.deR
.ft1
..
'\"	---------------	 HEADINGS  -------------------
.deH
.if\\n(:F .FE
.if\\n(:y .DE
.LC 0
.ft1
.br
.)R
.nr;1 0\\$1
.if!0\\$1 .nr ;1 \\n(Hu 
.if2-\\n(;1 .nr H2 0 1
.if3-\\n(;1 .nr H3 0 1
.if4-\\n(;1 .nr H4 0 1
.if5-\\n(;1 .nr H5 0 1
.if6-\\n(;1 .nr H6 0 1
.if7-\\n(;1 .nr H7 0 1
.nr H\\n(;1 +1
.if!\\n(;1-1  .sp 2
.ds}0 \\n(H1.
.if0\\$1-1 .as }0 \\n(H2
.if0\\$1-2 .as }0 .\\n(H3
.if0\\$1-3 .as }0 .\\n(H4
.if0\\$1-4 .as }0 .\\n(H5
.if0\\$1-5 .as }0 .\\n(H6
.if0\\$1-6 .as }0 .\\n(H7
.as}0 \ \ 
.if!0\\$1 .ds }0 
.Hd 0\\$1 "\\*(}0\\$2"
'br
..
.deHU
.H 0 "\\$1" "\\$2"
..
'\"	--------------	LISTS ----------------
.deLB
.)L 0\\$1n 0\\$2n 0\\$3n "\\$4" "\\$5" "\\$6" "\\$7"
..
.de)L
.if\\n(:g .)A 
.if!\\n+(:g-1 .ds ]b \\n(.i
.nr:b \\n(.iu+0\\$1u 
.nr:e 0\\$4 
.nr:f 0\\$6 
.if!\w~\\$6~ .nr :f 1
.ds]g \\$5
.if!\w~\\$5~ \{.ds ]g \&
.if\\n(:e .ds ]g 1
'br\}
.nr:a 0 1
.fi
.in\\n(:bu
.ti\\n(:bu
..
.deLC
.if\\n(:g-0\\$1 .)B
.if\\n(:g-0\\$1 .LC 0\\$1
..
.deLE
.ie!\\n(:g<1 .)B
.if(\\n(:g<=6)&(\\n(.$>0) .sp
..
'\"	PUSHDOWN STACK SAVE
.de)A
.ds]a \\n(:a \\*(]a
.ds]b \\n(:b \\*(]b
.ds]e \\n(:e \\*(]e
.ds]f \\n(:f \\*(]f
.ds]h \\*(]g \\*(]h
..
'\"	PUSHDOWN STACK RESTORE
.de)B
.br
.nr:g -1 1
.)C nr :a ]a \\*(]a
.nr :a \\n(:a 1
.)C nr :b ]b \\*(]b
'in\\n(:bu
'ti\\n(:bu
.)C nr :e ]e \\*(]e
.)C nr :f ]f \\*(]f
.)C ds ]g ]h \\*(]h
..
.de)C
.\\$1 \\$2 \\$4
.ds\\$3 \\$5 \\$6 \\$7 \\$8 \\$9
..
'\"	---------- LIST MEMBER -------------
.deLI 
.in\\n(:bu
.if\\n(:F 'in 0
.ds}0 \\*(]g
.if\\n(:e .ds }0 \\n+(:a.
.if\\n(:e-1 .ds }0 \\n(:a)
.if\\n(:e-2 .ds }0 (\\n(:a)
.if\\n(:e-3 .ds }0 [\\n(:a]
.if\\n(:e-4 .ds }0 <\\n(:a>
.if\\n(:e-5 .ds }0 {\\n(:a}
.if\\n(.$-1 .ds }0 \\$1\ \\*(}0
.if\\n(.$=1 .ds }0 \\$1
.nr;0 \w~\\*(}0~
.nr;1 0
.nr ;1 \\n(:bu-\\n(;0u-2n
.if!\\n(;1 .nr ;1 0
.nr;0 \\n(:bu-\\n(;1u-\\n(;0u
.ti\\n(;1u
.if\w~\\*(}0~ \&\\*(}0\\ \\ \&\c
..
'\"	-------------- LIST START MACRO ------------
.deAL
.if\\n(.$<3 \{.ie \w~\\$2~=0 .LB 10 0 0 1 "\\$1" 
.el.LB 0\\$2 0 0 1 "\\$1" \}
..
.deBL
.nr;0 10
.if(\\n(.$>0)&(\w~\\$1~>0) .nr ;0 0\\$1
.LB \\n(;0 0 0 0 "\\*(BU"
.rr;0
..
.deDL
.nr;0 10
.if(\\n(.$>0)&(\w~\\$1~>0) .nr ;0 0\\$1
.LB \\n(;0 0 0 0 "\(em"
.rr;0
..
.deML
.nr;0 \w~\\$1~u/3u/\\n(.su+1u
.ie\\n(.$<2 .LB \\n(;0 0 0 0 "\\$1"
.el .LB 0\\$2 0 0 0 "\\$1"
..
.deRL
.nr;0 12
.if(\\n(.$>0)&(\w~\\$1~>0).nr ;0 0\\$1
.LB \\n(;0 0 0 4 
.rr;0
..
.deVL
.LB 0\\$1 0\\$2 0 0
..
'\"	--------------  PARAGRAPH  --------------------
.deP
.br
..
.denP
.P
..
'\"	-------------- PAGES ----------------
.deSK
.bp
.br
..
'\"	-------------- SPACE ----------------
.deSP
.br
.sp \\$1
..
'\"	--------------- TABLES --------------
.deTS
.if\\n(;t .TE
.nr;t 1
.nr;i \\n(.i
.nr;q \\n(.u
.nf
.in0
.Ct
..
.deTH
..
.deTE
.Et
.br
.sp1
.in\\n(;iu
.fi
.if!\\n(;q .nf
.nr;t 0
..
'\"	------------ CAPTIONS -----------
.deEC
.)F Equation \\n+(Ec "\\$1" "\\$2"
..
.deEX
.)F Exhibit \\n+(Ex "\\$1" "\\$2"
..
.deFG
.)F Figure \\n+(Fg "\\$1" "\\$2"
..
.deTB
.)F TABLE \\n+(Tb "\\$1" "\\$2"
..
.de)F
.in0 
.Ce1
\f3\\$1 \\$2.\ \ \fP\\$3
.in
.Ce0
..
'\"	----------- DATE -----------
.deND
.dsDT "\\$1
..
'\"	------------ TITLE ---------------
.deTL
.nr;z 0 
'nr;y 1
'fi
.br
.rs
.sp1
.ft3
AT&T Bell Laboratories
'ft1
.br
'sp
.in 25
.ti -7
subject: 
.ft3
..
'\"	-------------- AUTHOR ----------------
.deAU
.if\\n(;y .>9 
.rmTL
.in 25
.ie!\\n(;z \{
'br
.ti -7
.ft 1
from:\ \ \ 
.ft 3 \}
.el .sp
\\$1
.br
\\$3\ \\$4
.br
\\$6\\ x\\$5
'br
.if\\n(.$-6 \\$7
.if\\n(.$-7 \\$8
.if\\n(.$-8 \\$9
.br
.nr;z 1
..
.de>9
'br
.fi
.sp
'ft1
.in25
.ti -7
date: 
.ft3
\\*(DT
'ft1
.nr;y 0
.rm>9
..
'\"	--------------- ABSTRACT --------------
.deAS
.if\\n(;y .>9
.rmTL
.sp3
.in 0
.ft 1
.Hd 8 ABSTRACT
.nf
.br
.fi
.rmAS
..
.deAE
.br
.rs
.in
.rmAE
..
'\"	--------------- NOTATIONS -----------
.deNS
.)R
.sp
.ie!\\n(!N Copy to:
.el .sp
.nf
.nr!N 1  
..
.deNE
.br
.nr!N 2
.)R 
..
'\"	-------------- MEMORANDUM TYPE --------------
.deMT
.if\\n(!N=1 .NE
.if!\\n(;y 'nf
.if\\n(;y .>9 
.rmTL
.rmAS
.rmAE
.rmAU
.if!\\n(.$ .>6 "TECHNICAL MEMORANDUM"
.if\\n(.$ .if \w~\\$1~u-\w'0'u .>6 "\\$1"
.if\\n(.$ .nr ;y 0\\$1
.if\\n(.$ .if !\\n(;y .>6 ""
.if\\n(.$ .if \\n(;y-4 .>6 "TYPE 4"
.if\\n(.$ .if \\n(;y-3 .>6 "TYPE 5"
.if\\n(.$ .if \\n(;y-2 .>6 "ADMINISTRATIVE MEMORANDUM"
.if\\n(.$ .if \\n(;y-1 .>6 "INTERNAL MEMORANDUM"
.if\\n(.$ .if \\n(;y .>6 "TECHNICAL MEMORANDUM"
.)R
.ns
.]N \\nP+1
.rmMT
..
.de>6 
.Hd 8 "\\$1"
.sp3 
.rm>6
..
'\"	----------- FOOTNOTES ---------
.deFS
'nr:s +1 
.if\\n(:F .FE
.if\\n(:y .DE
.nr:F 1
.if!\\n(!F .nr !F 1
.Fn 1
..
.deFE
.Fn 0
.nr:F 0
..
.deFD
..
'\"	----------- GC ------------
.deGS
.if!\\n(:y \{ .nr :G 1
.DS \}
.ft L
.Gc 1
..
.deGE
.if\\n(:G \{ .nr :G 0
.DE \}
.ft
.Gc 0
..
'\"	----------- PIC ------------
.dePS
.if!\\n(:P \{ .nr :P 1
.br
.Ps 1
.di >D \}
..
.dePE
.if\\n(:P \{ .nr :P 0
.di
.Ps 0
.br \}
..
'\"	------------- DISPLAYS -----------
.deDS
.)J "\\$1" "\\$2"
..
.deDF
.)J "\\$1" "\\$2"
..
.de)J
.if\\n(:F .FE
.if\\n(:y .DE
.nr;i \\n(.i
.nr;q \\n(.u
.nr ;H \\n(.f
.ft L
.nr:y 1
.br
.)R
.if~\\$1~I~ .in 5n
.if~\\$1~C~ .Ce1
.if~\\$1~CB~ .in 5n
.nf
.if~\\$2~F~ .fi
..
.deDE
.Ce0
.fi
.in\\n(;iu
.if!\\n(;q .nf
.ft\\n(;H
.nr:y 0
..
'\"	---------- GENERAL RESET FUNCTION -----------
.de)R
'fi
'in0
'ti0
..
'\"	----------- CONSTANT WIDTH ------------
.deCW
.DS I
..
.deCN
.DE
..
'\"	----------- REFERENCES ------------
.deRS
.if\\n(;R=2 .RF
.nr;R 2 
.nrRf +1 
.Fn 1
..
.deRF
.Fn 0
.nr;R 1
..
.deRP
.if\\n(;R=2 .RF
.if!(0\\$1) .nr :R 0 1 
.)R
.sp1
.Ce1
.ft2
REFERENCES
.ft
.Ce0
.sp2
.)R
'br
..
.dePM
..
.nr:y 0 
.nr:P 0
.nr:F 0
.nr:R 0 1 
.ds]r \\n(:R
.nr:p 0 1 
.ds]y \\n(:p
.nr W 6.0i
.if\n(mo-0 .ds DT January
.if\n(mo-1 .ds DT February
.if\n(mo-2 .ds DT March
.if\n(mo-3 .ds DT April
.if\n(mo-4 .ds DT May
.if\n(mo-5 .ds DT June
.if\n(mo-6 .ds DT July
.if\n(mo-7 .ds DT August
.if\n(mo-8 .ds DT September
.if\n(mo-9 .ds DT October
.if\n(mo-10 .ds DT November
.if\n(mo-11 .ds DT December
.asDT " \n(dy, 19\n(yr
