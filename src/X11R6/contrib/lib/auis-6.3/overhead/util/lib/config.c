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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/config.c,v 2.20 1993/07/08 19:09:23 Zarf Exp $";
#endif


 

#include <stdio.h>
#include <andrewos.h>	/* index */
#include <andyenv.h>	/* LOCAL_ANDREW_SETUP_ENV */
#include <system.h>	/* LOCAL_ANDREW_SETUP_ENV */
#include <ctype.h>
#include <errno.h>
#include <util.h>
#include <andrdir.h>

char ProgramName[100];
extern int errno;

#define MAXCONFIGSIZE 2000

char *conf_ConfigNames[] =  {
    "/AndrewSetup",
    "/etc/AndrewSetup",
#ifdef LOCAL_ANDREW_SETUP_ENV
    LOCAL_ANDREW_SETUP_ENV ,
#endif /* LOCAL_ANDREW_SETUP_ENV */
    "/usr/vice/etc/AndrewSetup",
/* Include a name based on DEFAULT_ANDREWDIR_ENV */
    QUOTED_DEFAULT_ANDREWDIR_ANDREWSETUP,
    "/usr/andrew/etc/AndrewSetup",
    NULL
};
int conf_ConfigUsed = -1;
int conf_ConfigErrno = -1;

/* 

getconfiguration -- read information from configuration file /AndrewSetup.

*/


ReadConfigureLine(fp, text, maxTextLength, program, programLength, key, keyLength, value, valueLength, condition, conditionLength)
FILE *fp;
char *text;
int maxTextLength;
char **program;
int *programLength;
char **key;
int *keyLength;
char **value;
int *valueLength;
char **condition;
int *conditionLength;
{
    char *keybeg;
    char *keyend;
    char *valpos;
    char *valend;
    char *programBeg;
    char *programEnd;
    static char *thisHost = NULL;

    if ((fgets(text, maxTextLength, fp)) != NULL) {
	if (text[0] == '#' || text[0] == '!')  {
	    return CONFIG_COMMENT;
	}
	if (text[0] == '?')  {
	    int matchIt;

	    /* Check for Machine Type / Host Name/ Environment variable  */

	    if (text[1] == 'C' || text[1] == 'M' || text[1] == 'E')  {
		register char *p;
		register char *d;

		p = &(text[2]);

		/* Test for which comparison */

		if (*p == '=')  {
		    matchIt = 1;
		} else if (*p == '!')  {
		    matchIt = 0;
		} else  {
		    return CONFIG_BADENTRY;
		}

		/* Get token */

		p++;
		while (isspace(*p))  {
		    p++;
		}
		d = p;
		while (*p && *p != ':')  {
		    if (*p == '\\' && *(p+1)) {
 			char *p2;
 			for (p2 = p; *p2; ++p2) {
 			    *p2 = *(p2+1);
 			}
 		    }
		    p++;
		}

		keybeg = p + 1;

		if (p == d)  {
		    return CONFIG_BADENTRY;
		}

		p--;

		while (p != d && isspace(*p))  {
		    p--;
		}

		if (condition != NULL && conditionLength != NULL)  {
		*condition = d;
		*conditionLength = p - d + 1;
		}

		/* Do proper comparison */

		if (text[1] == 'C')  {
		    int eq = FoldedEQn(d, SYS_NAME, p-d+1);
		    int eq2 = FoldedEQn(d, OPSYSNAME, p-d+1);

		    if ((!matchIt || ! eq) && (matchIt || eq) && (!matchIt || ! eq2) && (matchIt ||  eq2))  {
			return CONFIG_FALSECONDITION;
 		    }
 		} else if (text[1] == 'E') {
 		    char *val, *envar, *enval;
 		    int eq;
 
 		    val = index(d, '=');
 		    if (!val) return CONFIG_BADENTRY;
 		    envar = (char *) malloc(val - d + 1);
 		    if (!envar) return CONFIG_BADENTRY;
 		    strncpy(envar, d, val - d + 1);
 		    envar[val-d] = '\0';
 		    enval = (char *) getenv(envar);
		    free(envar);
 		    if (!enval) {
 			if (matchIt) return CONFIG_FALSECONDITION;
 		    } else {
 			++val;
 			eq = FoldedEQn(val, enval, keybeg - val -1);
 			if ((matchIt && !eq) || (!matchIt && eq)) return CONFIG_FALSECONDITION;
		    }
		} else {
		    if (thisHost == NULL)  {
			thisHost = (char *) malloc(256);
			if (thisHost != NULL) GetHostDomainName(thisHost, 256);
		    }

		    if (thisHost == NULL || (!matchIt || !FoldedEQn(d, thisHost, p-d+1)) && (matchIt || FoldedEQn(d, thisHost, p-d+1)))  {
			return CONFIG_FALSECONDITION;
		    }
		}
	    } else {
		return CONFIG_BADENTRY;
	    }
	} else  {
	    keybeg = text;
	    if (condition != NULL && conditionLength != NULL)  {
		*condition = NULL;
		*conditionLength = 0;
	    }
	}

	/* Skip over leading white space */

	while (*keybeg && isspace(*keybeg))  {
	    keybeg++;
	}

	if (*keybeg == '\0')  {
	    return CONFIG_EMPTYLINE;
	}

	programBeg = keybeg;
	programEnd = NULL;

	keyend = keybeg;

	/* Search for program name and key */

	if (program != NULL && programLength != NULL)  {
	    *program = NULL;
	    *programLength = 0;
	}

	while (*keyend && *keyend != ':')  {
	    if (*keyend == '.' && programEnd == NULL)  {
		/* Found program name - Null terminate string and move keybeg */

		if (keyend != programBeg)  {
		    programEnd = keyend-1;
		    while (programEnd != programBeg && isspace(*programEnd))  {
			programEnd--;
		    }
		    programEnd++;
		} else {
		    programEnd = programBeg;
		}

		if (program != NULL && programLength != NULL)  {
		    *program = programBeg;
		    *programLength = programEnd - programBeg;
		}

		/* Reset key beginning  and skip white space */

		keybeg = ++keyend;
		while (*keybeg && isspace(*keybeg))  {
		    keybeg++;
		}
		keyend = keybeg;
	    } else  {
		keyend++;
	    }
	}

	if (*keyend == '\0' || keyend == keybeg)  {
	    return CONFIG_NOKEY;
	}

	valpos = keyend + 1;

	/* strip off white space from key */

	keyend--;
	while (keyend != keybeg && isspace(*keyend))  {
	    keyend--;
	}

	keyend++;

	if (key != NULL && keyLength != NULL)  {
	    *key = keybeg;
	    *keyLength = keyend - keybeg;
	}

	/* Strip off white space from value */

	while (*valpos != '\0' && isspace(*valpos))
	    valpos++;

	if (*valpos)  {
	    valend = &(valpos[strlen(valpos) - 1]);
	    while (valend != valpos && isspace(*valend))
		valend--;

	    /* save if there is any value associated with entry */

	    valend++;

	    if (value != NULL && valueLength != NULL)  {
		*value = valpos;
		*valueLength = valend - valpos;
	    }

	    return CONFIG_FOUNDENTRY;
	} else
	    return CONFIG_NOVALUE;

    } else {
	return CONFIG_EOF;
    }
}

struct configurelist *ReadConfigureFile(fileName)
    char *fileName;
{
    FILE *fp;

    errno = 0;
    fp = fopen(fileName, "r");
    if (fp) {
	char mybuf[MAXCONFIGSIZE];
	char *key;
	char *program;
	char *value;
	int keyLength;
	int programLength;
	int valueLength;
	struct configurelist *newItem;
	struct configurelist *conHead = NULL;
	struct configurelist *conEnd = NULL;
	int retVal;

	while ((retVal = ReadConfigureLine(fp, mybuf, MAXCONFIGSIZE, &program, &programLength, &key, &keyLength, &value, &valueLength, NULL, NULL)) != CONFIG_EOF) {
	    if (retVal == CONFIG_FOUNDENTRY)  {
		newItem = (struct configurelist *) malloc(sizeof(struct configurelist));
		if (newItem == NULL)  {
		    fclose(fp); return NULL;
		}
		if ((newItem->key = (char *) malloc(keyLength + 1)) == NULL || (newItem->value = (char *) malloc(valueLength + 1)) == NULL)  {
		    fclose(fp); return NULL;
		}
		strncpy(newItem->key, key, keyLength);
		newItem->key[keyLength] = '\0';
		strncpy(newItem->value, value, valueLength);
		newItem->value[valueLength] = '\0';
		if (program != NULL && programLength != 0 && *program != '*')  {
		    if ((newItem->programName = (char *) malloc(programLength + 1)) == NULL)  {
			fclose(fp); return NULL;
		    }
		    strncpy(newItem->programName, program, programLength);
		    newItem->programName[programLength] = '\0';
		} else {
		    newItem->programName = NULL;
		}
		if (conHead == NULL)  {
		    conHead = newItem;
		} else {
		    conEnd->next = newItem;
		}
		newItem->next = NULL;
		conEnd = newItem;
	    }
	}

	fclose(fp);

	return conHead;
    }
    return NULL;
}
    

char *GetConfig(header, key, usedefault)
    struct configurelist *header;
    char *key;
    int usedefault;
{
    struct configurelist *p;
    char *t;
    char *testName;
    char pName[500];

    if (header == NULL || key == NULL || *key == '\0')
        return NULL;
    
    t = (char *) index(key, '.');

    if (t != NULL)  {
	strncpy(pName, key, t - key);
	pName[t-key] = '\0';
	key = t + 1;
	testName = pName;
    } else {
	testName = ProgramName;
    }

    for (p = header; p != NULL; p = p->next) {
	if (FoldedEQ(p->key, key) && ((usedefault && p->programName == NULL) || (p->programName != NULL && FoldedEQ(p->programName, testName))))  {
	    return (p->value);
	}
    }
    return NULL;
}

char *GetConfiguration(key)
char *key;
{
    static int inited = 0;
    static struct configurelist *setupHead = NULL;

    if (! inited) {
	int i;

	for (i= 0; conf_ConfigNames[i]; ++i) {
	    setupHead = ReadConfigureFile(conf_ConfigNames[i]);
	    if (setupHead != NULL || errno == 0)
		break;	/* setupHead will be NULL and errno be 0 if we
			    could fopen() the file but not malloc() enough space
			    to hold its contents. */
	}
	conf_ConfigErrno = errno;
	inited = 1;
	conf_ConfigUsed = i;
    }
    if (setupHead == NULL) {errno = conf_ConfigErrno; return NULL;}

    errno = 0;
    return GetConfig(setupHead, key, 1);
}

FreeConfigureList(cList)
    register struct configurelist *cList;
{
    register struct configurelist *t;

    while (cList != NULL)  {
	t = cList;
	cList = t->next;
	if (t->programName != NULL)
	    free(t->programName);
	if (t->key != NULL)
	    free(t->key);
	if (t->value != NULL)
	    free(t->value);
	free(t);
    }
}

/* This is the main routine used to test the routine above */

#ifdef TESTINGONLYTESTING
main(argc, argv)
int argc;
char **argv;
{
    int i;
    char *val;
    struct configurelist *ch = NULL;
    char *ConfFile = NULL;

    for (i = 1; i < argc; i++)  {
	if (argv[i][0] == '-')  {
	    if (argv[i][1] == 'f')  {
		ConfFile = &(argv[i][2]);
		ch = ReadConfigureFile(ConfFile);
		if (ch == NULL) {
		    printf("Cannot read specified file ``%s'': errno %d", ConfFile, errno);
		    if (errno == 0) printf("; probably not a configuration file");
		    else if (errno == ENOENT) printf(" (no such file)");
		    else if (errno == EACCES) printf(" (permission denied)");
		    printf("\n");
		    ConfFile = NULL;
		}
	    } else if (argv[i][1] == 'p') {
		strcpy(ProgramName,&(argv[i][2]));
	    }
	} else {
	    if (ch == NULL)  {
		val = GetConfiguration(argv[i]);
	    } else {
		val = GetConfig(ch, argv[i], 1);
	    }
	    printf("Configuration info for '%s' is '%s'\n", argv[i], (val == NULL ? "NULL" : val));
	}
    }
    if (ConfFile == NULL) {
	if (conf_ConfigUsed >= 0 && conf_ConfigNames[conf_ConfigUsed] != 0) {
	    printf("(Values were read from setup file %d, named ``%s''.)\n", conf_ConfigUsed, conf_ConfigNames[conf_ConfigUsed]);
	} else {
	    if (conf_ConfigUsed >= 0) {
		printf("No setup file could be found (resulting index was %d; errno %d).\nThe path used was:\n", conf_ConfigUsed, conf_ConfigErrno);
	    } else {
		printf("No setup file search performed.  Setup file search path is:\n");
	    }
	    for (i = 0; conf_ConfigNames[i]; ++i) printf(" [%d]\t%s\n", i, conf_ConfigNames[i]);
	}
    } else {
	printf("(Values were read from configuration file ``%s''.\n", ConfFile);
    }
}
#endif /* TESTINGONLYTESTING */
