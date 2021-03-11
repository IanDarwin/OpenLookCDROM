/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/sgi_mips/RCS/dofix.c,v 1.5 1994/02/28 04:47:50 rr2b Exp $";
#endif

/* 
	dofix.c - convert .o file into .do file

	Author:  Zalman Stern July 1989
 */

/* This version of dofix is much more simpler than ones for other machine types
 * since MIPS' ld does what we want...
 */

#include <stdio.h> /* For NULL in absence of stddef.h */

#include <sys/fcntl.h>
#include <filehdr.h>
#include <scnhdr.h>

static char *ComputeOutputFileName (InputFileName, extension)
char *InputFileName;
char *extension;
{

    static char name[256];
    register char  *p, *q;
    char   *ext;

 /* copy the input name and look for the last '.' */

    for (p = InputFileName, q = name, ext = NULL; *p != '\0';) {
	if (*p == '/')		/* ignore period if '/' follows */
	    p++, q = name, ext = NULL;
	else
	    if ((*q++ = *p++) == '.')
		ext = q - 1;
    }
    if (ext == NULL)
	ext = q;
    *ext = '\0';

 /* overwrite the extension with new extension */

    strncat(name, extension, 255);
    if (strcmp(InputFileName, name) == 0)
	strncat(name, extension, 255);
    return name ;
}

static char *argv0 = "dofix" ;

static void
ErrorExit( char *msg )
{
	(void) fprintf( stderr, "%s: %s\n", argv0, msg ) ;
	exit( -1 ) ;
	/*NOTREACHED*/
}

static unsigned long int
ComputeBindingAddress( InputFileName )
	char *InputFileName ;
{
	unsigned long int coreaddr ;
	int fd ;
	struct filehdr fh ;
	struct scnhdr sh ;
	int hash ;
	char *ptr ;

	fd = open( InputFileName, O_RDONLY ) ;
	if ( fd < 0 )
		ErrorExit( "Can't open Input File" ) ;

	if ( read( fd, &fh, sizeof fh ) != sizeof fh )
		ErrorExit( "Can't read file header" ) ;
	lseek( fd, fh.f_opthdr, 1 ) ;	/* past optional hdr */
	if ( read( fd, &sh, sizeof sh ) != sizeof sh )
		ErrorExit( "Can't read section header" ) ;

	(void) close( fd ) ;
	/* Calculate "hashed" address for binding */
	for ( hash = 0, ptr = InputFileName ; *ptr ; ++ptr ) {
		hash += hash >> 4 ; /* Really ( x * 17 ) / 16 */
		hash += *ptr ;
	}
	/* Allow the section to range from 4M to 192M in 4M increments */
	hash %= 47 ;

	return ( ( hash + 1 ) * 0x00400000 ) + sh.s_scnptr ;
}

main(argc, argp)
int argc;
char **argp;
{
    char *InputFileName;
    char *OutputFileName;
    char *EntryPointName = NULL;
    int gotcha = 0;
    int textaddr ;
    char CommandBuffer[1024];
    char *debugsymflag = "-x" ; /* Strip locals by default */

	argv0 = *argp ;

    while (--argc > 0) {
	if (**++argp == '-') {
	    switch (*++*argp) {
	    case 'T' :
		(void) sscanf( *++argp, "%x", &textaddr ) ;
		argc--;
		break;
	    case 'g':
		debugsymflag = "-g" ;
		break;
	    case 'd':
		break;
	    case 'e':
		if (*++*argp)
		    EntryPointName = *argp;
		else {
		    EntryPointName = *++argp;
		    argc--;
		}
                if (EntryPointName[0] == '_')
                    EntryPointName++;
		break;
	    default:
		fprintf(stderr, "dofix:  Unknown switch -%c ignored\n", *argp);
	    }
	}
	else {
	    gotcha++;
            InputFileName = *argp;
	    OutputFileName = ComputeOutputFileName(InputFileName, ".do");
        }
    }
    if (gotcha == 0) {
        InputFileName = "-";
        InputFileName = "-";
    }
    else if ( textaddr == 0 )
        textaddr = ComputeBindingAddress( InputFileName ) ;

    (void) sprintf( CommandBuffer, "ld -jumpopt %s -r -T %x -e %s %s -o %s",
		    debugsymflag, /* "-x" or "-g" -- Strip locals by default */
		    textaddr, EntryPointName,
		    InputFileName, OutputFileName ) ;
    if(system(CommandBuffer)!=0) {
      fprintf(stderr, "dofix: Couldn't fix up.\n");
      unlink(OutputFileName);
    } else {
	/* We must extend the .do file to contain enough space to hold the bss data.  Otherwise
	 doload may not be able to map enough space for unitialized static data. -rr2b */
	FILE *fp;
	/* this command looks through the .do file produced and determines the total space needed by bss and sbss. */
      sprintf(CommandBuffer, "size %s|awk '$1==\".bss\" || $1==\".sbss\" {total+=$2}\\\nEND { print total+0}'", OutputFileName);
      fp=popen(CommandBuffer, "r");
      if(fp==NULL) {
	unlink(OutputFileName);
	fprintf(stderr, "dofix: Couldn't get bss and sbss sizes.\n");
      } else {
	  /* read out the amount of space needed. */
	CommandBuffer[0]='\0';
	fgets(CommandBuffer, sizeof(CommandBuffer), fp);
	if(CommandBuffer[0]<'0' || CommandBuffer[0]>'9') {
	  unlink(OutputFileName);
	  fprintf(stderr, "dofix: Bad format output from bss and sbss size commands.\n");
	} else {
	  int len=atoi(CommandBuffer);
	  if(len!=0) {
	    int fd=open(OutputFileName, O_WRONLY, 0);
	    if(fd<0) {
	      unlink(OutputFileName);
	      fprintf(stderr, "dofix: Couldn't open %s for writing.\n", OutputFileName);
	    } else {
		/* extend the .do file to include space for the bss. */
		int r=lseek(fd, len, SEEK_END);
		CommandBuffer[0]='\0';
	      if(r<0 || write(fd, CommandBuffer, 1)!=1 || r<0 || close(fd)!=0) {
		unlink(OutputFileName);
		fprintf(stderr, "dofix: Couldn't extend %s by %d bytes.\n", OutputFileName, len);
	      }
	    }
	  }
	}
	pclose(fp);
      }
    }
    exit(0);
}
