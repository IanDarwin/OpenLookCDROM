/* C++ language routines for reading, writing, allocating, etc. data nodes */

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
Person::Person(char *Gname, char *Sname,
	char *Bdate, char *Bplace, char *Ddate, char *Dplace) {
	setName(Gname, Sname);
	bdate = Bdate;
	bplace = Bplace;
	ddate = Ddate;
	dplace = Dplace;
	sibling = next = 0;
	// parents = 0;
	// couple = 0;
}

Person::Person(char *Gname, char *Sname) {
	setName(Gname, Sname);
	bdate = 0;
	bplace = 0;
	dplace = 0;
	ddate = 0;
	sibling = next = 0;
	// parents = 0;
	// couple = 0;
}

Person::~Person() { }

void Person::setName(char* Gname, char *Sname) {
	gname = Gname;
	sname = Sname;
	buildName();
}

char * Person::GetName() {
	return pname;
}

Couple::Couple(Person *h, Person *w) {
	husband = h;
	wife = w;
}
Couple::~Couple() {
}
void Couple::AddKid(Person *k) { 
}

#if	0
insertPerson(p, dosort)
Person *p;
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
#endif

/* Get one person from the window system */
Person *
getPersonFromScreen(genix_rootsWindow_objects *ip)
{
	char *gname = strdup((char*)xv_get(ip->gnameTF, PANEL_VALUE, 0));
	char *sname = strdup((char*)xv_get(ip->surnameTF, PANEL_VALUE, 0));
	char *bdate = strdup((char*)xv_get(ip->bdateTF, PANEL_VALUE, 0));

	Person p(gname, sname, bdate, 0, 0, 0);

	return &p;
}

/*
 * Build up in "pname" a printable name that fits the
 * scrolling list but also lines up reasonably (assuming a monospaced font!!).
 */
void
Person::buildName()
{
	register int i, n;
	register char *s;
	static char spaces[PRTNAMELEN] = "                   "; /* ALIGN */

	strcpy(pname, spaces);
	for (i=0, s=gname; i<PRTNAMELEN && *s; i++,s++)
		pname[i] = *s;	/* strncpy without null byte */
	if ((n = strlen(gname)) < SNOFFSET)
		strncpy(pname+SNOFFSET, sname, PRTNAMELEN-SNOFFSET);
	else
		strncpy(pname+n+1, sname, PRTNAMELEN-(n+1));
}

void
readObjs(char *f) 
{
	int fd;
	struct stat statbuf;
	char *s, *t;
	off_t filesize;
	char *Sname, *Gname, *Bdate, *Bplace, *Ddate, *Dplace;

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

	for (t = s; t < s + filesize; ) {
		Sname = Gname = Bdate = Bplace = Ddate = Dplace = 0;

#define INPUT_FIELD(STRING, TAG) { \
		if (strncmp(t, STRING, sizeof(STRING)-1) == 0) { \
			t += sizeof(STRING)-1; \
			while (isspace(*t)) ++t; \
			TAG = t; \
			while (*t != '\n') ++t; \
			*t++ = '\0'; \
			} \
		}
		INPUT_FIELD("SN:", Sname);
		INPUT_FIELD("GN:", Gname);
		INPUT_FIELD("BD:", Bdate);
		INPUT_FIELD("BP:", Bplace);
		INPUT_FIELD("DD:", Ddate);
		INPUT_FIELD("DP:", Dplace);

		if (*t++ == '\n') {	/* i.e., null line */
			Person p(Sname, Gname, Bdate, Bplace, Ddate, Dplace);
		}
	}

	sort_lists();	/* i.e., do this once, not after each insert. */

	Changed = 1;
};


#if	0
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

#endif
