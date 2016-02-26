/* C-language routines for reading, writing, allocating, etc. data nodes */

#ifndef	lint
static char ident[] = "$Id: objs.c,v 1.9 91/10/30 12:46:02 ian Exp $";
#endif

#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include "genix_ui.h"
extern genix_relationsWindow_objects   *Genix_relationsWindow;
#include "genix.h"

/*
 * Allocate, delete, & otherwise manipulate objects.
 */
#define PERSON Person
PERSON *
newPerson() 
{
	PERSON *p = (PERSON *)malloc(sizeof(PERSON));

	if (p) {
		p->gname = p->sname = NULL;
		p->pname[0] = '\0';
		p->sibling = p->next = NULL_PERSON;
		p->parents = p->couple = NULL_COUPLE;
	}
	return p;
}


void
freePerson(p) 
PERSON *p;
{
	if (p)
		free(p);
};

insertPerson(p, dosort)
PERSON *p;
int dosort;
{
	int i;

	if (p->gname == NULL || p->sname == NULL || p->bdate == NULL ||
	    p->gname[0] == 0 || p->sname[0] == 0 || p->bdate[0] == 0) {
		warning("Must have firstname, lastname and bdate");
		freePerson(p);
		return -1;
	}

	if (p->pname[0] == '\0' || p->pname[0] == ' ')
		build_pname(p);

	for (i=0; i<np; i++)
		if (personcmp(&allp[i], &p) == 0) {
			freePerson(allp[i]);
			allp[i] = p;
		}

	if (np < MAXPEOPLE)
		allp[np++] = p;
	else {
		warning("Too many people!! Not Inserted!\n");
		return -1;
	}
	if (dosort)
		sort_lists();
	Changed = 1;

	return 0;
}

/* Get one person from the window system */
PERSON *
getPersonFromScreen(ip)
genix_rootsWindow_objects *ip;
{
	PERSON *p = newPerson();

	if (p) {
		p->gname = strsave(xv_get(ip->gnameTF, PANEL_VALUE));
		p->sname = strsave(xv_get(ip->surnameTF, PANEL_VALUE));
		p->bdate = strsave(xv_get(ip->bdateTF, PANEL_VALUE));

		build_pname(p);

	}

	return p;
}

/*
 * Build up in "pname" a printable name that fits the
 * scrolling list but also lines up reasonably (assuming a monospaced font!!).
 */
build_pname(p)
PERSON *p;
{
	register int i, n;
	register char *s;
	static char spaces[PRTNAMELEN] = "                   "; /* ALIGN */

	strcpy(p->pname, spaces);
	for (i=0, s=p->gname; i<PRTNAMELEN && *s; i++,s++)
		p->pname[i] = *s;	/* strcpy without null byte */
	if ((n = strlen(p->gname)) < SNOFFSET)
		strncpy(p->pname+SNOFFSET, p->sname, PRTNAMELEN-SNOFFSET);
	else
		strncpy(p->pname+n+1, p->sname, PRTNAMELEN-(n+1));
}

void
readObjs(f) 
char *f;
{
	int fd;
	PERSON *p;
	struct stat statbuf;
	char *s, *t;
	unsigned long filesize;

	if (stat(f, &statbuf)<0 || (fd = open(f, 0)) == 0) {
		warning("file not readable");
		return;
	}

	filesize = statbuf.st_size;

	/* strategy: malloc huge buffer using len from stat,
	 * then walk in with pointers. Save hundreds of mallocs!
	 */
	s = emalloc(filesize);

	if (read(fd, s, filesize) != filesize)
		error("read failed");
	(void) close(fd);

	for (t = s, p = newPerson(); t < s + filesize; ) {

#define INPUT_FIELD(STRING, TAG) { \
		if (strncmp(t, STRING, sizeof(STRING)-1) == 0) { \
			t += sizeof(STRING)-1; \
			while (isspace(*t)) ++t; \
			p->TAG = t; \
			while (*t != '\n') ++t; \
			*t++ = '\0'; \
			} \
		}
		INPUT_FIELD("SN:", sname);
		INPUT_FIELD("GN:", gname);
		INPUT_FIELD("BD:", bdate);
		INPUT_FIELD("BP:", bplace);
		INPUT_FIELD("DD:", ddate);
		INPUT_FIELD("DP:", dplace);

		if (*t++ == '\n') {	/* i.e., null line */
			insertPerson(p, 0);
			p = newPerson();
		}
	}

	sort_lists();	/* i.e., do this once, not after each insert. */

	Changed = 1;
};


void
writeObjs(f)
char *f;
{
	FILE *fp;
	PERSON *p;
	int i;

	if (fstat(fp, 0)) {
		/* then should prompt before overwriting */
	}
	if ((fp = fopen(f, "w")) == NULL) {
		/* then notice_prompt that it failed */
	}

	for (i=0; i < np; i++) {
		p = allp[i];
		if (p->sname && *(p->sname))
			fprintf(fp, "SN:%s\n", p->sname);
		if (p->gname && *(p->gname))
			fprintf(fp, "GN:%s\n", p->gname);
		if (p->bdate && *(p->bdate))
			fprintf(fp, "BD:%s\n", p->bdate);
		if (p->bplace && *(p->bplace))
			fprintf(fp, "BP:%s\n", p->bplace);
		fprintf(fp, "\n");
	}

	(void) fclose(fp);

	Changed = 0;
};


/*
 * String compare, first on surname, then on given name.
 * Used from qsort; if calling from open code, don't forget
 * to say personcmp(&p1, &p2); !!
 */
int
personcmp(pp1, pp2)
PERSON **pp1, **pp2;
{
	register int i;
	register PERSON *p1 = *pp1, *p2 = *pp2;

	if ((i=strcmp(p1->sname, p2->sname)) != 0)
		return i;
	else return strcmp(p1->gname, p2->gname);
}


sort_lists()
{
	PERSON *p;
	int i;
	static int oldnpers = 0;

	qsort(allp, np, sizeof(PERSON*), personcmp);

	/* try to put it into window system's scrolling list */
	xv_set(Genix_relationsWindow->leftList, XV_SHOW, FALSE, NULL);
	xv_set(Genix_relationsWindow->rightList, XV_SHOW, FALSE, NULL);
	for (i=0; i<np; i++) {
		xv_set(Genix_relationsWindow->leftList,
			PANEL_LIST_DELETE, i,
			PANEL_LIST_INSERT, i,
			PANEL_LIST_STRING, i, allp[i]->pname,
			NULL);
		xv_set(Genix_relationsWindow->rightList,
			PANEL_LIST_DELETE, i,
			PANEL_LIST_INSERT, i,
			PANEL_LIST_STRING, i, allp[i]->pname,
			NULL);
	}
	xv_set(Genix_relationsWindow->leftList, XV_SHOW, TRUE, NULL);
	xv_set(Genix_relationsWindow->rightList, XV_SHOW, TRUE, NULL);
}

char *emalloc(n)
unsigned n;
{
	char *p = (char *)malloc(n);

	if (!p) {
		fprintf(stderr, "Out of memory, requested %u bytes\n");
		exit(1);
	}
	return p;
}

char *
strsave(s)
char *s;
{
        char *emalloc(), *strcpy();

        return (s == (char*)0)? s: strcpy(emalloc((unsigned)strlen(s)+1), s);
}

