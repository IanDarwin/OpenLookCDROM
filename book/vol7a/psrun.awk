#!/bin/nawk -f
#
# psrun - run several DSC-conforming PostScript files - not necessarily
# all from one application - into one "book.ps" file. Assumes that you're
# running the PS interpreter on your workstation or can download all the
# files to the printer's disk.

# Ideas from Adobe via Charles Poynton's tech notes.

# Assuming the input may not have pdfmarks in it, we add a "pdfmark" like this:
# [/Page 27 /View [/Fit] /Title (Preface) /OUT pdfmark
# for each file, to make a minimal chapter-level TOC.
# The subject line comes from the file "BOOKFILES" described below.

BEGIN	{		## START OF LARGE BEGIN ACTION
	if (ARGC < 1) {
		print "Usage: psrun"
		exit 1
	}

	BF = "book.ps"
	print "%!PS-Adobe" > BF
	print "" >> BF
	# Next 4 lines must agree with like lines in toc-linked.ps!
	print "/cropLLX 25 def" >> BF
	print "/cropLLY 90 def" >> BF
	print "/cropURX 520 def" >> BF
	print "/cropURY 760 def" >> BF
	print "/TopOfPage cropURY def" >> BF
	print "[/CropBox [cropLLX cropLLY cropURX cropURY] /PAGES pdfmark" >> BF
	print "" >> BF

	# TODO can we use "startjob" instead of all this?
	print "/prun {/mysave save def RunFile clear cleardictstack mysave restore} def" >> BF

	# variable initializations...
	pagenum = 1	# avoid "page zero" problems

	# KLUDGE - stick in note to user about AcroRead/Acroexch.
	while (getline < "acronote.ps" == 1) {
		print >> BF
	}

# BOOKFILE contains a list of files in the book and, for each, a *short*
# description of that chapter. MUST BE ONE TAB between each filename
# and its descriptor. Example:
# toc.ps Contents
# ch00.ps Preface
# ch01.ps 1. XView and X
# ch02.ps 2. XView Programmer's Model

	# TODO error checking
	while (getline < "BOOKFILES" == 1) {
		fname = $1
		title = ""
		for (i=2; i<=NF; i++)
			title = title " " $i
		titles[fname] = title
	}

	}		## END OF LARGE "BEGIN" ACTION

function toc_entry(fname, title, pnum) {
	print "% psrun - inclusion of " fname	>> BF
	print "[/Page", pnum, "/View [/XYZ -4 TopOfPage 0] " >> BF
	sub(/^ /, "", title)	# chop leading space if any
	print "	/Title (" title ") /OUT pdfmark" >> BF
	print "(" fname ") prun"	>> BF
}

# Fix for Vol7A/B books  - book had title pages but we didn't get them
function blankPages() {
	for (ql=1;ql<=2;ql++) {
		print "%%Page:", pagenum++, "dummy" >> BF
		print "/Helvetica findfont 12 scalefont setfont" >> BF
		print "200 600 moveto (This page intentionally left blank) show" >> BF
		print "200 550 moveto (to preserve original page counts.) show" >> BF
		print "showpage" >> BF
		print "%%EndPage" >> BF
		}
	}
FNR == 1 {
	# Insert blank pages for the "Chapter Headers" in the printed book.
	# Only do so if filename is ch*, sec* or app* - O'Reilly-specific.
	t = substr(FILENAME, 1, 2)
	if (t == "ch" || t == "ap" || t == "se")
		blankPages()
	if (FILENAME == "ch25.ps")
		blankPages()	# work around a bug in the PS files??
	toc_entry(FILENAME, titles[FILENAME], pagenum)
	}

/^%%Page:/ {$3 = ++pagenum; relpagnum = $2}

END	{
	print "%%EOF" >> BF
	}
