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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/machdep/sgi_mips/RCS/doload.c,v 1.4 1992/12/15 20:59:52 rr2b R6tape $";
#endif

/*
	doload.c - dynamic loader for class system

	Author:  Zalman Stern July 1989
 */

/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/*
	doload.c - dynamic loader for class system

	Author:  Zalman Stern July 1989
 */

#include <sys/types.h>
#include <sys/signal.h>
#include <unistd.h>
#include <sys/file.h>
#include <sys/mman.h>
#include <sys/cachectl.h>
#include <errno.h>
#include <stdio.h>
#include <a.out.h>
#include <setjmp.h>
#include <unistd.h>

#include <andrewos.h> /* sys/types.h */

#define ALIGNMENTSIZE 4
#define _DYDDX_INTERNAL
#include "doload.h"

#ifndef CRUFT_GONE
int doload_trace = 0 ;
#endif
extern int _gp ; /* contains address in gp register */

#if !defined MIPSEB && !defined MIPSEL
	****** ERROR -- MUST BE ONE OR THE OTHER !!
#endif /* !defined MIPSEB && !defined MIPSEL */

#if defined MIPSEB
#define HighShortOf( longIntPtr ) ( ( (short int *)( longIntPtr ) )[0] )
#define LowShortOf( longIntPtr ) ( ( (short int *)( longIntPtr ) )[1] )
#else /* !defined MIPSEL */
#define HighShortOf( longIntPtr ) ( ( (short int *)( longIntPtr ) )[1] )
#define LowShortOf( longIntPtr ) ( ( (short int *)( longIntPtr ) )[0] )
#endif

/* Yes, we really dynamically link shared libraries ! */
#define CHECK_SHLIB

#include "../common/safe.h"

/* Forward declaration */
void *(* doload( int, char *, char **, long int *, char * ) )() ;

extern struct gsym {
	void *entrypoint ;	/* entry point value */
	char *name ;		/* symbolic name */
} globals[] ;
extern long int globalcount ;

/* tear down environment */
static void
doload_cleanup( e )
	register struct doload_environment *e ;
{
	if ( e->problems > 0 ) {
		e->problems = 0 ;
		doload_punt( e, "Errors while processing" ) ;
		/*NOTREACHED*/
	}
	safe_free( (char *) e->sections ) ;
	safe_free( (char *) e->rtab ) ;
	safe_free( (char *) e->symtab ) ;
	safe_free( e->stringtab ) ;
	return ;
}

static char *
read_section(
	struct doload_environment *e,
	long offset,
	char *buffer,
	long length )
{
	if ( !buffer )
		buffer = (char *) safe_malloc( e, length ) ;

	if ( lseek( e->fd, offset, L_SET ) == -1 ) {
		doload_punt( e, "seek failed" ) ;
		/*NOTREACHED*/
	}
	safe_read( e, buffer, length ) ;
	return buffer ;
}

/* check if shared library was statically linked in to this image */
static struct _sigc { jmp_buf jb ; } * volatile caughtSEGV = 0 ;
static volatile unsigned long int junk ;

static void
SEGVhandler()
{
	struct _sigc *returnto = caughtSEGV ;
	caughtSEGV = (struct _sigc *) 0 ;
	longjmp( returnto->jb, 1 ) ;
	/*NOTREACHED*/
}

static int
CheckForStaticSharedLibrary( e, textaddr )
	register struct doload_environment *e ;
	register unsigned int textaddr ;
{
	struct _sigc segv_recover ;
	int (*oldsighndlr)() ;
	char checkbuf[1024] ;

	oldsighndlr = signal( SIGSEGV, (int (*)()) SEGVhandler ) ;
	caughtSEGV = &segv_recover ;
	if ( !setjmp( segv_recover.jb ) ) {
		junk = * (unsigned long int *) textaddr ;
		(void) read_section( e, e->sections[0].s_scnptr,
				     checkbuf, sizeof checkbuf ) ;
	}
	(void) signal( SIGSEGV, oldsighndlr ) ;
	return caughtSEGV != &segv_recover
		|| bcmp( (void *) textaddr, checkbuf, sizeof checkbuf ) ;
}

/* mmap shared library into memory */
/* WARNING: very simplistic */
static void
shlib_mmap( e, totalMmapSize, totalBssSize )
	register struct doload_environment *e ;
	register int totalMmapSize ;
	register int totalBssSize ;
{
	register unsigned int textcore ;
	register unsigned int totalSize ;
	register char *membase ;

	/* MUST map the file where it believes it goes */
	textcore = e->sections[0].s_vaddr ;

	/* Check to see if we can "jmp" that far from here */
	if ( ( textcore & 0xF0000000 )
	  != ( ( (int) shlib_mmap ) & 0xF0000000 ) ) {
		doload_punt( e, "Shared library TEXT not reachable!" ) ;
		/*NOTREACHED*/
	}

	/* See if it is already in core */
	if ( CheckForStaticSharedLibrary( e, textcore ) == 0 )
		return ;

	/* This fails if the TEXT section's data isn't first in the object */
	membase = mmap( (char *) textcore, e->sections[0].s_size,
			PROT_READ | PROT_EXECUTE, MAP_FIXED | MAP_SHARED,
			e->fd, e->sections[0].s_scnptr ) ;

	if ( textcore != (unsigned int) membase ) {
		doload_punt( e, "Failed mmap() - shared library TEXT !" ) ;
		/*NOTREACHED*/
	}
	e->text = membase ;

	/* Adjust totalMmapSize for already mapped section */
	totalMmapSize += e->sections[0].s_scnptr - e->sections[1].s_scnptr,

	/* This fails if the DATA section's data isn't second in the object */
	membase = mmap( (char *) e->sections[1].s_vaddr, totalMmapSize,
			PROT_READ | PROT_WRITE, MAP_FIXED | MAP_PRIVATE,
			e->fd, e->sections[1].s_scnptr ) ;
	if ( e->sections[1].s_vaddr != (unsigned int) membase ) {
		munmap( e->text, e->sections[0].s_size ) ;
		doload_punt( e, "Failed mmap() - shared library DATA !" ) ;
		/*NOTREACHED*/
	}

	/* Build the BSS section. Sorry, no demand fill, we're not the kernel */
	bzero( membase + totalMmapSize - totalBssSize, totalBssSize ) ;

	return ;
}

struct shlibDesc {
	struct shlibDesc *next ;
	char path[1] ; /* A lie, but ... */
} ;

/* Initialize as NULL !! */
static struct shlibDesc *sharedLibraryList = (struct shlibDesc *) 0 ;
static struct shlibDesc *sharedLibraryListEnd = (struct shlibDesc *) 0 ;

static int
CheckSharedLibrary( lppath )
	register char *lppath ;
{
	register struct shlibDesc *shptr ;

	for ( shptr = sharedLibraryList ; shptr ; shptr = shptr->next ) 
		if ( !strcmp( lppath, shptr->path ) )
			return 0 ;
	return 1 ;
}

static void
RememberSharedLibrary( e, lppath )
	register struct doload_environment *e ;
	register char *lppath ;
{
	register struct shlibDesc *shptr ;
	register int len ;
	register struct shlibDesc *prevshptr ;

	len = strlen( lppath ) + 1 ;
	shptr = (struct shlibDesc *)
		safe_malloc( e, len + sizeof (struct shlibDesc) ) ;
	shptr->next = (struct shlibDesc *) 0 ;
	(void) strncpy( shptr->path, lppath, len ) ;
	if ( !( prevshptr = sharedLibraryListEnd ) )
		sharedLibraryListEnd = sharedLibraryList = shptr ;
	else {
		sharedLibraryListEnd->next = shptr ;
		sharedLibraryListEnd = shptr ;
	}
	return ;
}

/* Recurse to bind all referenced shared libraries */
static void
dbindSharedLibraries( e, tempSection )
	register struct doload_environment *e ;
	register struct scnhdr *tempSection ;
{
	register struct libscn *lp ;
	char *lppath ;
	char *lpend ;
	struct libscn *lpstart ;
	int fd ;
	void *(*entry)() ;

	/* read the list of shared libraries */
	lpstart = lp = (struct libscn *)
	 read_section( e, tempSection->s_scnptr, NULL, tempSection->s_size ) ;
	lpend = ( (char *) lp ) + tempSection->s_size ;

	while ( lpend > (char *) lp ) {
		if ( lp->tsize ) {
			lppath = (char *) ( ( (long int *) lp ) + lp->offset ) ;
			if ( CheckSharedLibrary( lppath ) ) {
				fd = open( lppath, O_RDONLY ) ;
				if ( fd < 0 ) {
					safe_free( (char *) lpstart ) ;
					doload_punt( e,
						"Can't find shared library!" ) ;
					/*NOTREACHED*/
				}
				entry = doload( fd, 0, 0, 0, 0 ) ;
				(void) close( fd ) ;
				if ( !entry ) {
					safe_free( (char *) lpstart ) ;
					doload_punt( e,
						"Can't bind shared library!" ) ;
					/*NOTREACHED*/
				}
				RememberSharedLibrary( e, lppath ) ;
			}
		}
		lp = (struct libscn *) ( ( (long int *) lp ) + lp->size ) ;
	}
	safe_free( (char *) lpstart ) ;

	return ;
}

/* mmap module into memory */
static void
doload_mmap( e, totalMmapSize, totalBssSize )
	register struct doload_environment *e ;
	register int totalMmapSize ;
	register int totalBssSize ;
{
	register unsigned long int pgsz ;
	register unsigned int coreaddr ;
	register unsigned int totalSize ;
	register char *membase ;
	register int offset ;

	/* Pagesize damn well better be a power of 2 */
	pgsz = getpagesize() ;
	/* Assume twos complement ( it is a mips, after all ) */
	if ( ( pgsz & - pgsz ) != pgsz ) {
		doload_punt( e, "Pagesize NOT a power of 2!" ) ;
		/*NOTREACHED*/
	}

	/* Now cheat: make a bit mask! */
	pgsz = -pgsz ; /* !! */

	/* Starting point in file to map */
	offset = e->sections[0].s_scnptr & pgsz ;

	/* Try to map the file where it believes it goes */
	coreaddr = e->sections[0].s_vaddr & pgsz ;

	/* Check to see if we can "jmp" that far from here */
	if ( ( coreaddr & 0xF0000000 )
	  != ( ( (int) doload_mmap ) & 0xF0000000 ) ) {
		coreaddr &= 0x0FFFFFFF ;
		coreaddr |= ( (int) doload_mmap ) & 0xF0000000 ;
	}

	e->totalsize = totalMmapSize + e->sections[0].s_scnptr - offset ;

	/* This fails if the TEXT section's data isn't first in the object */
#ifdef ORIGINAL
	membase = mmap( (char *) coreaddr, e->totalsize,
			PROT_READ | PROT_WRITE | PROT_EXECUTE,
			MAP_FIXED | MAP_PRIVATE,
			e->fd, offset ) ;

	if ( membase == (char *) -1 ) /* Try again -- any old address */
#endif
		membase = mmap( 0, e->totalsize,
				PROT_READ | PROT_WRITE | PROT_EXECUTE,
				MAP_PRIVATE,
				e->fd, offset ) ;

	/* Now round up so that we can munmap() later */
	e->totalsize += ~ pgsz ;
	e->totalsize &= pgsz ;

	e->membase = membase ;
	e->text = membase + e->sections[0].s_scnptr - offset ;

	if ( membase == (char *) -1 ) {
		doload_punt( e, "Failed mmap() !" ) ;
		/*NOTREACHED*/
	}

	/* Build the BSS section. Sorry, no demand fill, we're not the kernel */
	bzero( e->text + totalMmapSize - totalBssSize, totalBssSize ) ;

	return ;
}

static char *
gp_append( register struct doload_environment *e, int size )
{
	char *gptr ;

	gptr = (char *) &sdpool[ sdpoolstart ] ;
	sdpoolstart += ( ( size + 3 ) >> 2 ) ; /* Round up to long ints */
	return gptr ;
}

/* This routine reads all of the interesting sections of a object module into
memory so they can be worked on by other parts of the code. Some of these
sections are permanent while others are only used during the loading phase and
are freed afterwards. (List specific sections used by this code.)
*/
static void
doload_read( register struct doload_environment *e )
{
	register struct scnhdr *tempSection ;
	register int count ;
	register int tmp ;
	struct scnhdr *lastRealSection ;
	long int totalRelocs ;
	unsigned long int totalMmapSize ;
	unsigned long int totalBssSize ;
	unsigned int totalGpSize ;
	int gpOffset ;
	int initSection ; /* Is there a ".init" section and where is it ? */
	int lastGpSection = -1;

	/* read header */
	safe_read( e, (char *) &(e->filehdr), (long) sizeof e->filehdr ) ;
	safe_read( e, (char *) &(e->aouthdr), (long) sizeof e->aouthdr ) ;

	/* Calculate the total size of the section headers and scratch */
	tmp = e->filehdr.f_nscns * sizeof (struct scnhdr) ;
	count = tmp + ( e->filehdr.f_nscns * sizeof (char *) ) ;

	/* Malloc space and zero the scratch pointers */
	e->sections = (struct scnhdr *) safe_malloc( e, count ) ;
	e->contents = (char **) ( ( (char *) e->sections ) + tmp ) ;
	bzero( (char *) e->contents, count - tmp ) ;

	/* really read the section headers */
	(void) read_section( e, sizeof (struct filehdr) + e->filehdr.f_opthdr,
			     (char *) e->sections, tmp ) ;

	/* Insist on TEXT ??? */
	if ( strcmp( e->sections[0].s_name, _TEXT )
	  || e->sections[0].s_size == 0 ) {
		doload_punt( e, "Missing or empty text section!" ) ;
		/*NOTREACHED*/
	}
	initSection = -1 ;
	totalRelocs = 0 ;
	totalBssSize = 0 ;

	e->firstGpSection = -1 ;
	totalGpSize = 0 ;

	for ( lastRealSection = tempSection = e->sections,
		count = e->filehdr.f_nscns ; count-- ; tempSection++ ) {

		/* Here we get a head start at calculating the eventual
		 * virtual address after loading into memory
		 */
		e->contents[ tempSection - e->sections ] =
			(char *) tempSection->s_vaddr ;

		totalRelocs += tempSection->s_nreloc ;

		if ( tempSection->s_flags & ( STYP_NOLOAD | STYP_DSECT ) )
			continue ;

		switch ( tempSection->s_flags & ~S_NRELOC_OVFL ) {
		case STYP_INIT :
			initSection = tempSection - e->sections ;
			break ;

		case STYP_TEXT :
		case STYP_RDATA :
		case STYP_DATA :
			break ;

		case STYP_BSS :
			totalBssSize += tempSection->s_size ;
			continue ; /* Doesn't count towards file area */

		case STYP_LIB :
			dbindSharedLibraries( e, tempSection ) ;
			continue ;

		case STYP_LIT8 :
		case STYP_LIT4 :
		case STYP_SDATA :
			lastGpSection = tempSection - e->sections ;
			if ( e->firstGpSection < 0 ) /* Is this the first ? */
				e->firstGpSection = lastGpSection ;
			totalGpSize += tempSection->s_size ;
			break ;

		case STYP_SBSS :
			lastGpSection = tempSection - e->sections ;
			if ( e->firstGpSection < 0 ) /* Is this the first ? */
				e->firstGpSection = lastGpSection ;
			totalGpSize += tempSection->s_size ;
			totalBssSize += tempSection->s_size ;
			continue ; /* Doesn't count towards file area */

#ifdef CHECK_UCODE
		case STYP_UCODE :
			doload_punt( e, "ucode section in dynamic object" ) ;
			/*NOTREACHED*/
#endif /* CHECK_UCODE */

		default :
			if ( tempSection->s_size ) {
				doload_punt( e,
	"doload: Unexpected non-zero section encountered." ) ;
				/*NOTREACHED*/
			}
			continue ;
		} /* End very ugly switch */
		/* Remember where the end is -- note: "continue"'ed from BSS */
		if ( tempSection->s_scnptr > lastRealSection->s_scnptr )
			lastRealSection = tempSection ;
	} /* End very ugly for */

	/* Calculate the size of the interesting area within the file */
	totalMmapSize = lastRealSection->s_scnptr + lastRealSection->s_size
		      + totalBssSize - e->sections[0].s_scnptr ;

	/* mmap() it now */
	if ( e->aouthdr.magic == LIBMAGIC )
		shlib_mmap( e, totalMmapSize, totalBssSize ) ;
	else {
		doload_mmap( e, totalMmapSize, totalBssSize ) ;

		/* Finish the calculation of each sections actual address */
		tmp = (int) e->text - e->sections[0].s_vaddr ;

		if ( e->gpSize = totalGpSize ) { /* Do gp Stuff */
			e->gpBase = (char *) gp_append( e, totalGpSize ) ;
			e->gpOffset = gpOffset = ( (int) e->gpBase )
				 - (int) e->contents[ e->firstGpSection ] ;
		}

		count = e->filehdr.f_nscns ;
		do {
			if ( --count >= e->firstGpSection
			  && count <= lastGpSection ) {
				/* Really move it */
				if (!( e->sections[count].s_flags & STYP_SBSS ))
					bcopy( e->contents[count] + tmp,
					       e->contents[count] + gpOffset,
					       e->sections[count].s_size ) ;
				e->contents[count] += gpOffset ;
			}
			else
				e->contents[count] += tmp ;
		} while ( count ) ;
	}
	/* if there exists an ".init" section, prepare to call into it */
	if ( initSection >= 0 )
		e->initpoint = (void (*)()) e->contents[ initSection ] ;

	/* Perhaps the reloactions should be read into two different areas? */
	if ( totalRelocs )
		e->rtab = (struct reloc *)
			read_section( e, e->sections[0].s_relptr, NULL,
				      totalRelocs * sizeof (struct reloc) ) ;

	/* Get the symbolic header */
	(void) read_section( e, e->filehdr.f_symptr, (char *) &e->symheader,
			     sizeof e->symheader ) ;

	/* Get scratch space for symbol split fix-ups */
	count = e->symheader.iextMax ;
	if ( count ) {
		count *= ( sizeof (EXTR) + ( 2 * sizeof (short) ) ) ;
		e->symtab = (pEXTR) safe_malloc( e, count ) ;

		/* Read the symbol table's symbols */
		(void) read_section( e, e->symheader.cbExtOffset, (char *) e->symtab,
				     e->symheader.iextMax * sizeof (EXTR) ) ;

		/* Patch the scratch space pointers */
		e->lastHighStart = (short *) ( ( (char *) e->symtab )
				 + ( e->symheader.iextMax * sizeof (EXTR) ) ) ;
		e->lastHighResult = e->lastHighStart + e->symheader.iextMax ;

		/* Finally allocate and read the string table */
		e->stringtab = (char *)
			read_section( e, e->symheader.cbSsExtOffset,
				      NULL, e->symheader.issExtMax ) ;
	}

	return ;
}

/* Binary search on already sorted symbol list */
static struct gsym *
findGlobalSymbol( register char *symname )
{
	register struct gsym *gsptr ;
	register struct gsym *gsptop ;

	for ( gsptr = globals, gsptop = globals + globalcount - 1 ;
	      gsptr <= gsptop ;
	      ++gsptr )
		if ( !strcmp( symname, gsptr->name ) )
			return gsptr ;
	return 0 ;
}

/* This routine rolls down the symbol table and finds those symbols which
 * also appear in the globals array. When it finds one, it fills in the
 * value with that from globals. If there are any undefined symbols left
 * after this routine executes, its an error.
 */
static void
doload_preset( e )
	register struct doload_environment *e ;
{
	register pEXTR sp ;
	register struct gsym *thisGlobal ;
	register unsigned long int length ;
	register unsigned long int commonSize ;

	commonSize = 0 ;
	for ( sp = e->symtab ; sp < e->symtab + e->symheader.iextMax ; sp++ ) {

		if ( sp->asym.sc == scUndefined || sp->asym.sc == scCommon
		  || sp->asym.sc == scSUndefined || sp->asym.sc == scSCommon ) {

			thisGlobal =
			  findGlobalSymbol(  e->stringtab + sp->asym.iss ) ;

			if ( thisGlobal ) {
				sp->asym.value = (int) thisGlobal->entrypoint ;
			}
			else if ( sp->asym.sc == scSCommon
			       || ( sp->asym.sc == scCommon
			       && sp->asym.value <= e->runTimeGnum ) ) {
				length = sp->asym.value ;
				sp->asym.value = (unsigned long)
					gp_append( e, length ) ;
				bzero( (char *) sp->asym.value, length ) ;
			}
			else if ( sp->asym.sc == scCommon
			       && sp->asym.value > 0 ) {
				length = sp->asym.value ;
				sp->asym.value = commonSize ;
				commonSize += (((length - 1) / ALIGNMENTSIZE) + 1) * ALIGNMENTSIZE;
				continue ; /* Don't set to scAbs ! */
			}
			else {
				char msg[256] ;
				(void) sprintf( msg,
						"No such symbol (%.200s)\n",
						e->stringtab + sp->asym.iss ) ;
				doload_punt( e, msg ) ;
				/*NOTREACHED*/
			}
			sp->asym.sc = scAbs ;
		}
		else if ( sp->asym.sc == scSData || sp->asym.sc == scSBss ) {
			sp->asym.value += e->gpOffset ;
			sp->asym.sc = scAbs;
		}
	}
	if ( commonSize ) {
		e->commonArea = safe_malloc( e, commonSize ) ;
		bzero( (char *) e->commonArea, commonSize ) ;
		for ( sp = e->symtab ;
		      sp < e->symtab + e->symheader.iextMax ;
		      sp++ )
			if ( sp->asym.sc == scCommon ) {
				sp->asym.value += (unsigned long)
					e->commonArea ;
				sp->asym.sc = scAbs ;
			}
	}
	return ;
}

/* relocate one item */

#define HI16MASK    0xFFFF0000
#define LOW16MASK   0x0000FFFF
#define LOW26MASK   0x03FFFFFF
#define HI6MASK     0xFC000000

#ifndef ORIGINAL
static int foundSC[100];
#endif

static void
doload_relocate( e, cp, rp, sectionRelocBound )
	register struct doload_environment *e ;
	register char *cp ;
	register struct reloc *rp ;
	struct reloc *sectionRelocBound ; /* Used for bounds checking below. */
{
	register long tw ;
	register long disp  = 0; /* "displacement" : how much it moved ! */
	register long original_tw ;

	if ( rp->r_extern ) {
	    disp = e->symtab[ rp->r_symndx ].asym.value;
	    switch (e->symtab[rp->r_symndx ].asym.sc) {
		case scNil :
		    disp = 0 ;
		    break ; /* an ABS gp symbol ? */

		case scText :
		case scRData :
		case scData :
		case scBss :
		    disp += (int) e->contents[ 0 ]
		      - e->sections[ 0 ].s_vaddr ;
		    break ;

		case scSData :
		case scSBss :
		    disp = (int) e->contents[ e->firstGpSection ]
		      - e->sections[ e->firstGpSection ].s_vaddr ;
		    break ;
		case scAbs:
		    /* These we don't have to do anything with */
		    break;
		default:
		    if (! foundSC[e->symtab[rp->r_symndx ].asym.sc]) {
			fprintf(stderr, "Found exteral relocation storage class %d\n", e->symtab[rp->r_symndx ].asym.sc);
			foundSC[e->symtab[rp->r_symndx ].asym.sc] = 1;
		    }
		    break;
	    }
	}
	else {
	    /* Either fixup to the patched gp section or the rest of it */
	    switch ( rp->r_symndx ) {
		case R_SN_NULL :
		    disp = 0 ;
		    break ; /* an ABS gp symbol ? */

		case R_SN_TEXT :
		case R_SN_RDATA :
		case R_SN_DATA :
		case R_SN_BSS :
		case R_SN_INIT :
		    disp = (int) e->contents[ 0 ]
		      - e->sections[ 0 ].s_vaddr ;
		    break ;

		case R_SN_SDATA :
		case R_SN_SBSS :
		case R_SN_LIT8 :
		case R_SN_LIT4 :
		    disp = (int) e->contents[ e->firstGpSection ]
		      - e->sections[ e->firstGpSection ].s_vaddr ;
		    break ;
	    }
	}

	if ( rp->r_type == R_GPREL || rp->r_type == R_LITERAL ) {
	    if (rp->r_extern && e->symtab[rp->r_symndx].asym.sc == scAbs) {
		disp -= (int) &_gp;
	    }
	    else {
		disp += e->aouthdr.gp_value - (int) &_gp ;
	    }
	}

	if ( !disp ) { /* No change ( we mmap'ed at a "good" address ) */
		e->prevType = rp->r_type ;
		return ;
	}

	switch ( rp->r_type ) {

	case R_REFHALF:
		* (short *) cp += disp ;
		break ;

	case R_REFWORD:
		* (long *) cp += disp ;
		break ;
	case R_JMPADDR:
		original_tw = * (long *) cp ;
		tw = ( original_tw & LOW26MASK ) << 2 ;
		tw += disp ;
		/* The REAL restriction on mips jumps is that both
		 * the target of the instruction and the instruction
		 * itself must lie within the same 28-bit segment.
		 * i.e. They must have the same high order nibble !
		 * This is significantly worse than having a +/- 2^27
		 * bit range.
		 */
		/* NOTE: this code is not really correct.
		 * we need to apply this test at the final
		 * application of the relocation.
		 * Hence this test is sometimes too restrictive
		 * and sometimes too lax, but its better than nothing.
		 */
		if ( ( tw >= 0x0FFFFFFF )
		  || ( tw <= - 0x10000000 )
		  || ( ( rp->r_vaddr + tw ) & 0xF0000000
		    != ( rp->r_vaddr + 4 ) & 0xF0000000 ) ) {
			doload_punt( e, "JUMPADDR relocation overflow." ) ;
			/*NOTREACHED*/
		}
		tw >>= 2 ;
		tw |= original_tw & HI6MASK ;
		* (long *) cp = tw ;
		break ;

	case R_REFHI:
		if ( rp >= sectionRelocBound
		  || ( rp + 1 )->r_type != R_REFLO ) {
			doload_punt( e,
			 "R_REFHI relocation record not followed by REFLO." ) ;
			/*NOTREACHED*/
		}

/* I assume that the low half of this will be appropriately sign extended. */
		tw = LowShortOf( rp->r_vaddr ) ;

		if ( !rp->r_extern )
			e->lastHighStart[rp->r_symndx] = tw ;
		else {
			/* I doubt this code ever gets used,
			 * but I don't know... */
			e->lastExternHighStart = tw ;
			e->lastExternSymndx = rp->r_symndx ;
		}
		/* Now get the bottom part */
		tw <<= 16 ;
		tw += LowShortOf( ( rp + 1 )->r_vaddr ) ;

		tw += disp ;
		/* Correct for being signed */
		tw += ( tw & ( 1 << 15 ) ) << 1 ;

		LowShortOf( rp->r_vaddr ) = tw >> 16 ;
		LowShortOf( ( rp + 1 )->r_vaddr ) = (short) tw ;

		if ( !rp->r_extern )
			e->lastHighResult[rp->r_symndx] = tw >> 16 ;
		else
			e->lastExternHighResult = tw >> 16 ;
		break ;

	case R_REFLO:
/* Apparently, sometimes relocation records don't come in pairs. This code
 * assumes that the only time this happens is when there are multiple references
 * to very close addresses. (In one particular case I found, this
 * happens when loading a double which requires two load instructions. A
 * REFHI is only emitted for the first one apparently.)
 * This code simply checks to see if this REFLO will work with
 * the REFHI of the last REFHI/REFLO pair and if so it puts in the result
 * value that was computed for the last pair.
 *
 * In actuality, things are more complicated than the above indicates. One
 * must pair the REFLO with the last REFHI from the same section. This hairs
 * up the code a great deal.
 *
 * Incidentally, if anyone from MIPS reads this, this is about the point where I
 * started re-evaluating my admiration of the R2000/R3000 and accompanying
 * software :-)
 */
		if ( e->prevType != R_REFHI ) {
			short highStart ;
			short highResult ;

			if ( !rp->r_extern ) {
				highStart = e->lastHighStart[rp->r_symndx] ;
				highResult = e->lastHighResult[rp->r_symndx] ;
			}
			else if ( rp->r_symndx == e->lastExternSymndx ) {
				highStart = e->lastExternHighStart ;
				highResult = e->lastExternHighResult ;
			}
			else {
				doload_punt( e,
"REFLO to an external symbol with different index than last extern REFHI.") ;
				/*NOTREACHED*/
			}

			tw = ( highStart << 16 ) + LowShortOf( rp->r_vaddr ) ;

			tw += disp ;
			/* Correct for being signed */
			tw += ( tw & ( 1 << 15 ) ) << 1 ;

			if ( ( tw >> 16 ) != highResult ) {
				doload_punt( e,
"R_REFLO relocation record not immediately preceeded by REFHI.\n\
 Situation cannot be resolved by using last REFHI." ) ;
				/*NOTREACHED*/
			}
			LowShortOf( rp->r_vaddr ) = (short) tw ;
		}
		break ;

	case R_GPREL:
	case R_LITERAL:
		{
			original_tw = tw = LowShortOf( rp->r_vaddr ) ;

			tw += disp ;
			if ( tw > 32767 || tw < -32768 ) {
				doload_punt( e,
"External R_GPREL relocation does not result in a gp addresable value." ) ;
				/*NOTREACHED*/
			}
			LowShortOf( rp->r_vaddr ) = tw ;
		}
		break ;
	}

	e->prevType = rp->r_type ;
	return ;
}

/* Adjust virtual addresses in relocation records to be zeroed where
 * the text segment was actually loaded into this process' VM.
 */
static void
doload_dorelocs( e )
	register struct doload_environment *e ;
{
	register struct reloc *rp ;
	register struct scnhdr *tmpSection ;
	register long int sectionOffset ;
	register int count ;
	register struct reloc *startRp ;

	rp = e->rtab ;
	tmpSection = e->sections ;
	for ( count = 0 ; count < e->filehdr.f_nscns ; count++, tmpSection++ ) {

		startRp = rp ;
		sectionOffset = tmpSection->s_vaddr
				   - (long) e->contents[count] ;
		if ( sectionOffset )
			for ( ; rp < startRp + tmpSection->s_nreloc ; rp++ )
				rp->r_vaddr -= sectionOffset ;

		for ( rp = startRp ;
		      rp < startRp + tmpSection->s_nreloc ;
		      rp++ ) {
			doload_relocate( e, rp->r_vaddr, rp,
			     startRp + tmpSection->s_nreloc ) ;
		}
	}

	if ( cacheflush( e->membase, e->totalsize, BCACHE ) ) {
		doload_punt( e, "Could not flush caches after doload." ) ;
		/*NOTREACHED*/
	}

	/* all done */
	return ;
}

static struct {
	void *base ;
	unsigned long int len ;
	void *gpBase ;
	unsigned long int gpSize ;
} lastLoaded ;
/*
 * read and relocate module
 * return the entry point or NULL if error
 */
void *(* doload(
			/* or NULL if error */
	int inFD,	/* open fd for package file */
	char *name,	/* name of package being loaded */
	char **bp,	/* base address of package */
	long *lenP,	/* size of text segment */
	char *path )	/* Pathname of package being loaded */
			/* Path is used by the MACH loader, not this one */
)()
{
	struct doload_environment E ;

	/* set up environment */
	bzero( &E, sizeof E ) ;
	E.runTimeGnum = 8 ; /* equivolent to ld -G xxx */
	E.fd = inFD ;
	/* For the following line :
	 * I know it gives a warning, but the problem is in the ".h", not here!
	 * The type "jmpbuf" should be a structure, not an array.
	 */
	if ( setjmp( E.errorJump ) ) {
		doload_cleanup( &E ) ;
		return NULL ;
	}

	/* read module into memory */
	doload_read( &E ) ;

	if ( E.aouthdr.magic != LIBMAGIC ) {
		/* replace imported symbol values */
		if ( E.symheader.iextMax != 0 )
			doload_preset( &E ) ;

		/* do relocation */
		doload_dorelocs( &E ) ;

		/* do runtime relocation of shared library imports */
		if ( E.initpoint )
			(* E.initpoint)() ;
	}

	/* all done */

	if (doload_trace)
	    printf( " %s: text = 0x%.8x  textsize = 0x%.8x\n",
		   name, E.membase, E.totalsize);
	/* all done */
	lastLoaded.base = E.membase ;
	lastLoaded.len = E.totalsize ;
	lastLoaded.gpBase = E.gpBase ;
	lastLoaded.gpSize = E.gpSize ;

	if ( bp )
		*bp = E.membase ;
	if ( lenP )
		*lenP = E.totalsize ;

	doload_cleanup( &E ) ;

	return (void *(*)())
		( E.text + E.aouthdr.entry - E.aouthdr.text_start ) ;
}

void
dounload( char *base, int len )
{
	if ( base == lastLoaded.base
	  && len == lastLoaded.len ) {
		munmap( lastLoaded.base, lastLoaded.len ) ;
		bzero( lastLoaded.gpBase, lastLoaded.gpSize ) ;
	}
	return ;
}
