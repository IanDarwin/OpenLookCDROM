/*	EPATH:	This file contains certain info needed to locate the
		MicroEMACS files on a system dependant basis.

									*/

/*	possible names and paths of help files under different OSs	*/

char *pathname[] =

#if	AMIGA
{
	".emacsrc",
	"emacs.hlp",
	"",
	"sys:c/",
	"sys:t/",
	"sys:s/",
	":c/",
	":t/",
	":s/"
};
#endif

#if	ST520
{
	"emacs.rc",
	"emacs.hlp",
	"\\",
	"\\bin\\",
	"\\util\\",
	""
};
#endif

#if	FINDER
{
	"emacs.rc",
	"emacs.hlp",
	"/bin",
	"/sys/public",
	""
};
#endif

#if	MSDOS
{
	"emacs.rc",
	"emacs.hlp",
	"\\sys\\public\\",
	"\\usr\\bin\\",
	"\\bin\\",
	"\\",
	""
};
#endif

#if	V7 | BSD | USG
{
	".emacsrc",
	"emacs.hlp",
	"/usr/local/",
	"/usr/lib/",
	""
};
#endif

#if	VMS
{
	"emacs.rc",
	"emacs.hlp",
	"",
	"sys$sysdevice:[vmstools]"
};
#endif

#define	NPNAMES	(sizeof(pathname)/sizeof(char *))
