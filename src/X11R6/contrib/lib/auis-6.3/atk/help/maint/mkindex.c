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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/help/maint/RCS/mkindex.c,v 2.39 1993/09/22 20:17:15 gk5g Exp $";
#endif

 
 
 
#define FSSIZE		32	/* max number of files */
#define BUCKETS		11	/* number of hash buckets */
#define MANSUBS "12345678nolpx"	/* array of possible subdirectories of MANDIR, ie man1, mann */

#include <andrewos.h> /* sys/types.h sys/file.h */ 
#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <index.h>

static char *prog;		/* our name */
static int verbose = 0;
static int action = 1;


/*
 * lowercases's a string.
 */
static char *LowerCase(astring)
register char *astring;
{
    register char *tp = astring;

    while (tp && *tp != (char)0)
	if (isupper(*tp)) {
	    *tp = tolower(*tp);
	    tp++;
	} else
	    tp++;
    return astring;
}


void AddPrimary(newIndex, key, path)
struct Index *newIndex;
char *key, *path;
{
    char *ap=CURRENTANDREWDIR;
    int alen=strlen(ap);
    char fullpath[MAXPATHLEN];
    if(strncmp(path, ap, alen)==0 && strlen(path)>alen && path[alen]=='/') {
	strcpy(fullpath, "$ANDREWDIR");
	strcat(fullpath, path+alen);
	index_AddPrimary(newIndex, key, fullpath);
    } else index_AddPrimary(newIndex, key, path);
    
}

static BuildIndex(aindex, srcDirName, targetDirName)
char *srcDirName, *targetDirName;
register struct Index *aindex;
{
    register DIR *srcDir;
    register DIRENT_TYPE *sde;
    register char *tp, *tfp, *sfp;
    char srcPath[MAXPATHLEN];
    char targetPath[MAXPATHLEN];
    char keyBuffer[MAXPATHLEN / 4];
    char mandir = 0;
    static char ManName[] = "man";
    struct stat tstat;
    register long code;
    
    if (!(srcDir = opendir(srcDirName))) {
        fprintf(stderr, "%s: can't open %s; skipping.\n", prog, srcDirName);
        return 2;
    } else {
	if (verbose)
	    printf("%s: indexing %s as %s.\n", prog, srcDirName, targetDirName);
    }

    tp = rindex(srcDirName, '/');
    mandir = (strcmp( (tp) ? tp+1 : srcDirName, ManName) == 0) ? 1 : 0;

    /* make a source base path */
    strcpy(srcPath, srcDirName);
    sfp = srcPath + strlen(srcPath);
    *sfp++ = '/';		/* sfp now points after the trailing / */

    /* make a target base path */
    strcpy(targetPath, targetDirName);
    tfp = targetPath + strlen(targetPath);
    *tfp++ = '/';		/* tfp now points after the trailing / */

    while(sde=readdir(srcDir)) {
        if (strcmp(sde->d_name, ".") == 0 || strcmp(sde->d_name, "..") == 0)
	    continue;
        strcpy(sfp, sde->d_name); /* complete the source path */
        strcpy(tfp, sde->d_name); /* complete the target path */

	strcpy(keyBuffer, sde->d_name);
        tp = strrchr(keyBuffer, '.');
        if (tp) *tp = '\0';           /* remove extension */
	/* check for "man" subdir */
        code = stat(srcPath, &tstat);
        if ((code == 0 && (tstat.st_mode & S_IFMT) == S_IFDIR) && (mandir != 0) && strncmp(sde->d_name, ManName, sizeof(ManName)-1) == 0) {
	    if(action > 0) 
		BuildIndex(aindex, srcPath, targetPath);
	} 
	else {
	    if (tp && index(MANSUBS, *(tp+1))) { /* it's a man page */
		/* add w/o extension */
		if(action > 0) 
		    AddPrimary(aindex, LowerCase(keyBuffer), targetPath);
		*tp = '.';	/* put back extension */
		/* add with extension */
		if(action > 0) 
		    AddPrimary(aindex, LowerCase(keyBuffer), targetPath);
	    } else {
		if ((code == 0 && (tstat.st_mode & S_IFMT) != S_IFDIR)) {
		    /* don't add directories */
		    if(action > 0) AddPrimary(aindex, LowerCase(keyBuffer), targetPath);
		}
	    }
	}
    }
    closedir(srcDir);
    return 0;
}


void show_usage()
{
    fprintf(stderr,"usage: %s [-n] [-v] input-file destination-index-dir\n", prog);
    fprintf(stderr,"\tinput file contains real-dir referencing-name pairs\n");
    fprintf(stderr,"\t-v: verbose mode\n");
    fprintf(stderr,"\t-n: fake-it mode\n");
}



main(argc, argv)
int argc;
register char **argv;
{
    struct Index *newIndex;
    long code, lineNo[FSSIZE];
    FILE *inputFile[FSSIZE], *tfile;
    char *inputName, *destinationName, *tmp;
    char opcode[MAXPATHLEN], path1[MAXPATHLEN], path2[MAXPATHLEN];
    int fsPtr;
 
    inputName = (char *) NULL;
    destinationName = (char *) NULL;
    tmp = rindex(*argv, '/');
    if (tmp)
	prog = tmp+1;
    else
	prog = *argv;

    while(*++argv!=NULL)
	if(**argv=='-')
	    switch((*argv)[1]){
		case 'n':
		    action = 0;
		    break;
		case 'v':
		    verbose = 1;
		    break;
		case 'x':
		case 'h':
		    show_usage();
		    exit(0);
		    break;
		case 'f':
		    fprintf(stderr,"%s: -f switch is now obsolete, continuing anyway\n", prog);
		default:
		    fprintf(stderr,"%s: unrecognized switch: %s\n", prog, *argv);
		    show_usage();
		    if ((*argv)[1] != 'f')
			exit(1);
		    break;
	    } else {
		if (inputName == NULL)
		    inputName = *argv;
		else if (destinationName == NULL)
		    destinationName = *argv;
		else {
		    show_usage();
		    exit(1);
		}
	    }

    if (destinationName == NULL || inputName == NULL) {
        show_usage();
	exit(1);
    }

    inputFile[0] = fopen(inputName, "r");
    fsPtr = 0;      /* current input file */

    if (!inputFile[0]) {
        fprintf(stderr,"%s: input file %s not found.\n", prog, inputName);
        exit(1);
    }
    
    /* now we create a destination index */
    if (verbose)
	printf("%s: refreshing index.\n", prog);

    if(action > 0) {
	index_Create(destinationName, BUCKETS);
	newIndex = index_Open(destinationName);
	/* now read each dir, and add the appropriate primary elements to newIndex */
	if (!newIndex) {
	    fprintf(stderr, "%s: could not create index %s\n", prog, destinationName);
	    exit(1);
	}
    }
    else {
	if(access(destinationName, W_OK) < 0) {
	    fprintf(stderr, "%s: could not create index %s\n", prog, destinationName);
	    exit(1);
	}
    }

    lineNo[fsPtr] = 0;	/* evidently Sun4.0 doesn't grok this --mrp */
    while (1) {
        lineNo[fsPtr]++;
        code = fscanf(inputFile[fsPtr], "%s", opcode);
        if (code <= 0) {
            fclose(inputFile[fsPtr]);
            if (fsPtr <= 0) break;
            if (verbose)
	        printf("%s: include file end.\n", prog);
            fsPtr--;
            continue;
        }
        if (opcode[0] == '#') {	/* comment */
	    int tc;

	    while (1) {
		tc = getc(inputFile[fsPtr]);
		if (tc <= 0 || tc == '\n')
		    break;
	    }
            continue;
        }

	/* dir to index */
        if (!strcmp(opcode, "dir")) {
            code = fscanf(inputFile[fsPtr], "%s %s", path1, path2);
            if (code != 2) {
                fprintf(stderr, "%s: input line %d: wrong number of parameters (%d should be 2) to dir operation\n", prog, code, lineNo[fsPtr]);
		exit(1);
            } else 
		BuildIndex(newIndex, path1, path2);
        }

	/* alias to add */
        else if (!strcmp(opcode, "key")) {
            code = fscanf(inputFile[fsPtr], "%s %s", path1, path2);
            if (code != 2) {
                fprintf(stderr, "%s: line %d: wrong number of parameters (%d should be 2) to key operation\n", prog, code, lineNo[fsPtr]);
		exit(1);
            } else {
		if (verbose)
		    printf("%s: mapping key '%s' to file %s\n", prog, LowerCase(path1), path2);
		if(action > 0)
		    AddPrimary(newIndex, LowerCase(path1), path2);
	    }
        }

	/* include file */
        else if (!strcmp(opcode, "include")) {
            code = fscanf(inputFile[fsPtr], "%s", path1);
            if (code != 1) {
                fprintf(stderr, "%s: line %d: syntax error in include command\n", prog, lineNo[fsPtr]);
                exit(1);
            }
            tfile = fopen(path1, "r");
            if (!tfile) {
                fprintf(stderr, "%s: line %d: include file %s not found.\n", prog, lineNo[fsPtr], path1);
                exit(1);
            }
            if (verbose)
		printf("%s: including subfile %s.\n", prog, path1);
            inputFile[++fsPtr] = tfile;
            lineNo[fsPtr] = 0;
        }
        else {
            printf("%s: line %d -- unknown opcode in input file '%s'\n", prog, lineNo[fsPtr], opcode);
            exit(1);
        }
    }
    if (verbose)
	printf("%s: storing index files.\n", prog);
    if(action > 0) index_Close(newIndex);
    return(0);
}
