/*
 * The routines in this file read and write ASCII files from the disk. All of
 * the knowledge about files are here.
 */

#include        <stdio.h>
#include	"estruct.h"
#include        "edef.h"

FILE	*ffp;		/* File pointer, all functions. */
int eofflag;		/* end-of-file flag */

/*
 * Open a file for reading.
 */
ffropen(fn)
char    *fn;
{
        if ((ffp=fopen(fn, "r")) == NULL)
                return (FIOFNF);
	eofflag = FALSE;
        return (FIOSUC);
}

/*
 * Open a file for writing. Return TRUE if all is well, and FALSE on error
 * (cannot create).
 */
ffwopen(fn)
char    *fn;
{
#if     VMS
        register int    fd;

        if ((fd=creat(fn, 0666, "rfm=var", "rat=cr")) < 0
        || (ffp=fdopen(fd, "w")) == NULL) {
#else
        if ((ffp=fopen(fn, "w")) == NULL) {
#endif
                mlwrite("Cannot open file for writing");
                return (FIOERR);
        }
        return (FIOSUC);
}

/*
 * Close a file. Should look at the status in all systems.
 */
ffclose()
{
	/* free this since we do not need it anymore */
	if (fline) {
		free(fline);
		fline = NULL;
	}

#if	MSDOS & CTRLZ
	fputc(26, ffp);		/* add a ^Z at the end of the file */
#endif
	
#if     V7 | USG | BSD | (MSDOS & (LATTICE | MSC | TURBO))
        if (fclose(ffp) != FALSE) {
                mlwrite("Error closing file");
                return(FIOERR);
        }
        return(FIOSUC);
#else
        fclose(ffp);
        return (FIOSUC);
#endif
}

/*
 * Write a line to the already opened file. The "buf" points to the buffer,
 * and the "nbuf" is its length, less the free newline. Return the status.
 * Check only at the newline.
 */
ffputline(buf, nbuf)
char    buf[];
{
        register int    i;
#if	CRYPT
	char c;		/* character to translate */

	if (cryptflag) {
	        for (i = 0; i < nbuf; ++i) {
			c = buf[i] & 0xff;
			crypt(&c, 1);
			fputc(c, ffp);
		}
	} else
	        for (i = 0; i < nbuf; ++i)
        	        fputc(buf[i]&0xFF, ffp);
#else
        for (i = 0; i < nbuf; ++i)
                fputc(buf[i]&0xFF, ffp);
#endif

#if	ST520
        fputc('\r', ffp);
#endif        
        fputc('\n', ffp);

        if (ferror(ffp)) {
                mlwrite("Write I/O error");
                return (FIOERR);
        }

        return (FIOSUC);
}

/*
 * Read a line from a file, and store the bytes in the supplied buffer. The
 * "nbuf" is the length of the buffer. Complain about long lines and lines
 * at the end of the file that don't have a newline present. Check for I/O
 * errors too. Return status.
 */
ffgetline()

{
        register int c;		/* current character read */
        register int i;		/* current index into fline */
	register char *tmpline;	/* temp storage for expanding line */

	/* if we are at the end...return it */
	if (eofflag)
		return(FIOEOF);

	/* dump fline if it ended up too big */
	if (flen > NSTRING) {
		free(fline);
		fline = NULL;
	}

	/* if we don't have an fline, allocate one */
	if (fline == NULL)
		if ((fline = malloc(flen = NSTRING)) == NULL)
			return(FIOMEM);

	/* read the line in */
        i = 0;
        while ((c = fgetc(ffp)) != EOF && c != '\n') {
                fline[i++] = c;
		/* if it's longer, get more room */
                if (i >= flen) {
                	if ((tmpline = malloc(flen+NSTRING)) == NULL)
                		return(FIOMEM);
                	strncpy(tmpline, fline, flen);
                	flen += NSTRING;
                	free(fline);
                	fline = tmpline;
                }
        }

#if	ST520
	if(fline[i-1] == '\r')
		i--;
#endif

	/* test for any errors that may have occured */
        if (c == EOF) {
                if (ferror(ffp)) {
                        mlwrite("File read error");
                        return(FIOERR);
                }

                if (i != 0)
			eofflag = TRUE;
		else
			return(FIOEOF);
        }

	/* terminate and decrypt the string */
        fline[i] = 0;
#if	CRYPT
	if (cryptflag)
		crypt(fline, strlen(fline));
#endif
        return(FIOSUC);
}

#if	AZTEC & MSDOS
#undef	fgetc
/*	a1getc:		Get an ascii char from the file input stream
			but DO NOT strip the high bit
*/

int a1getc(fp)

FILE *fp;

{
	int c;		/* translated character */

	c = getc(fp);	/* get the character */

	/* if its a <LF> char, throw it out  */
	while (c == 10)
		c = getc(fp);

	/* if its a <RETURN> char, change it to a LF */
	if (c == '\r')
		c = '\n';

	/* if its a ^Z, its an EOF */
	if (c == 26)
		c = EOF;

	return(c);
}
#endif
