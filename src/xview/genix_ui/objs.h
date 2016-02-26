/* Definitions file for genix objects. */

typedef struct person {
	char *gname;
	char *sname;
	char pname[PRTNAMELEN];		/* printable */
	char *bdate, *bplace;
	char *ddate, *dplace;
	/* etc. */
	struct couple *parents;		/* up-link */
	struct couple *couple;		/* self-wedding */
	struct person   *sibling;	/* next-link in child list */
	struct person *next;		/* next-link in main list */
} PERSON;
#define NULL_PERSON ((PERSON *)0)

/* A couple is any cohabital union that results in offspring;
 * no religious or moral overtones meant or intended.
 * "Just the facts, ma'am."
 */
typedef struct couple {
	PERSON *husband, *wife;
	PERSON *child;
} COUPLE;
#define NULL_COUPLE ((COUPLE *)0)
