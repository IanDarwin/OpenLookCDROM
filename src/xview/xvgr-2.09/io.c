/* $Id: io.c,v 1.12 92/08/15 15:55:11 pturner Exp Locker: pturner $
 *
 * input error checking, fexists()
 *
 */

#include <stdio.h>
#include <pwd.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>

static char readbuf[80];

int ibounds(x, lower, upper, name)
    int x, lower, upper;
    char *name;
{
    int test;

    test = ((x >= lower) && (x <= upper));
    if (!test) {
	sprintf(readbuf, " in %s : parameter must be in (%d , %d)", name, lower, upper);
	errwin(readbuf);
    }
    return (test);
}

int fbounds(x, lower, upper, name)
    double x, lower, upper;
    char *name;
{
    int test;

    test = ((x >= lower) && (x <= upper));
    if (!test) {
	sprintf(readbuf, "In %s : parameter must be in [%lf, %lf]", name, lower, upper);
	errwin(readbuf);
    }
    return (test);
}

int fexists(to)
    char *to;
{
    struct stat stto;
    char tbuf[256];

    if (stat(to, &stto) == 0) {
	sprintf(tbuf, "Overwrite %s?", to);
	if (!yesno(tbuf, "", "YES", "NO")) {
	    return (1);
	}
	return (0);
    }
    return (0);
}

int isdir(f)
    char *f;
{
    struct stat st;

    stat(f, &st);
    return ((st.st_mode & S_IFMT) == S_IFDIR);
}

int sortstrcmp(str1, str2)
    char **str1, **str2;
{
    return (strcmp(*str1, *str2));
}

#ifdef XVIEW

#define MAXLIST 2048
static char *dirlist[MAXLIST];
static int nentries;

char **make_dir_list(mask, nitems)
    char *mask;
    int *nitems;
{
    FILE *p;
    int d1, d2, i = 0;
    char buf[MAXPATHLEN + 17];
    int sortstrcmp();

    strcpy(buf, "/bin/ls -a ");
    if (mask != NULL) {
	strcat(buf, mask);
    }
    for (i = 0; i < nentries; i++) {
	cfree(dirlist[i]);
	dirlist[i] = NULL;
    }
    p = popen(buf, "r");
    if (p != NULL) {
	i = 0;
	while (fgets(buf, 255, p) != NULL) {
	    buf[strlen(buf) - 1] = 0;
	    if (strcmp(buf, ".")) {
		dirlist[i] = (char *) malloc(strlen(buf) + 1);
		strcpy(dirlist[i], buf);
		i++;
	    }
	}
	pclose(p);
	nentries = i;
	qsort(dirlist, nentries, sizeof(char *), sortstrcmp);
    } else {
	errwin("Can't open '%s' in files popup");
    }
    *nitems = nentries;
    if (nentries == 0) {
	return NULL;
    }
    else {
	return dirlist;
    }
}

int my_chdir(buf)
char *buf;
{
    int retval;
    if (!(retval = chdir(buf))) {
	 set_curdir();
    }
    return retval;
}

expand_tilde(buf)
    char buf[];
{
    char buf2[MAXPATHLEN];
    if (buf[0] == '~') {
        if (strlen(buf) == 1) {
            strcpy(buf, getenv("HOME"));
            strcat(buf, "/");
        } else if (buf[1] == '/') {
            strcpy(buf2, getenv("HOME"));
            strcat(buf2, "/");
            strcat(buf2, buf + 1);
            strcpy(buf, buf2);
        } else {
            char tmp[128], *pp = tmp, *q = buf + 1;
            struct passwd *pent;

            while (*q && (*q != '/')) {
                *pp++ = *q++;
            }
            *pp = 0;
            if ((pent = getpwnam(tmp)) != NULL) {
                strcpy(buf2, pent->pw_dir);
                strcat(buf2, "/");
                strcat(buf2, q);
                strcpy(buf, buf2);
            } else {
                errwin("No user by that name");
            }
        }
    }
}

#endif
