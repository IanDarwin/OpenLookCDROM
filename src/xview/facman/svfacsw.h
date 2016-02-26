/*
 * svfacsw -- System V Facility Switch
 * Turns a process started from /etc/init.d on or off
 */

/* For now one item, hardcoded. To be a list read from a file,
 * and to be display in a scrolling list too.
 */
struct Fac {
	char *name;	/* facility name */
	char *file;	/* name in init.d */
	char *rc23name;	/* to be **, list of link names. */
};

#ifdef	MAIN
struct Fac facility[] = {
	{ "Volume Manager", "volmgt", "rc2/S24volmgt" }
};
#endif

int start(struct Fac *f);
int stop(struct Fac *f);
