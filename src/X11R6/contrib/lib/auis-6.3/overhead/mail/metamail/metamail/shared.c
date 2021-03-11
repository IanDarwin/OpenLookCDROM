#include <stdio.h>
#include <ctype.h>
#include <config.h>
#ifdef SYSV
#include <unistd.h>
#include <sys/utsname.h>
#endif

extern char *malloc();
char **Exceptions;
int *NeedsPortableNewlines;
int ExceptionsAlloced = 0, ExceptionsUsed = 0;

ExceptionalNewline(contenttype, needsportable)
char *contenttype;
int needsportable;
{
    char *s;
    if (ExceptionsAlloced == 0) {
        ExceptionsAlloced = 25;
        Exceptions = (char **) malloc(ExceptionsAlloced * sizeof(char *));
        NeedsPortableNewlines = (int *) malloc(ExceptionsAlloced * sizeof(int));
        if (!Exceptions || !NeedsPortableNewlines) return(-1);
    }
    if (ExceptionsUsed >= ExceptionsAlloced) {
        ExceptionsAlloced += 25;
        Exceptions = (char **) realloc(Exceptions, ExceptionsAlloced * sizeof(char *));
        NeedsPortableNewlines = (int *) realloc(NeedsPortableNewlines, ExceptionsAlloced * sizeof(int));
        if (!Exceptions || !NeedsPortableNewlines) return(-1);
    }
    s = malloc(1+strlen(contenttype));
    if (!s) return(-1);
    strcpy(s, contenttype);
    Exceptions[ExceptionsUsed] = s;
    for (; *s; ++s) {
        if (isupper((unsigned char) *s)) *s = tolower((unsigned char) *s);
    }
    NeedsPortableNewlines[ExceptionsUsed] = needsportable;
    ++ExceptionsUsed;
    return(0);
}

DoesNeedPortableNewlines(ctype)
char *ctype;
{
    int i;

    /* First, handle the customization/override case */
    for (i=0; i<ExceptionsUsed; ++i) {
        if (!lc2strcmp(ctype, Exceptions[i])) {
             return(NeedsPortableNewlines[i]);
        }
    }
    /* But for most folks, the simple defaults will always suffice */
    if (!lc2strncmp(ctype, "text", 4)) {
        return(1);
    }
    /* The following two are weird; message & multipart should never be encoded, but they really are line-oriented, so if they ARE encoded (and for PEM, it is even legitimate) they should use portable newlines */
    if (!lc2strncmp(ctype, "message", 7)) {
        return(1);
    }
    if (!lc2strncmp(ctype, "multipart", 9)) {
        return(1);
    }
    return(0);
}

lc2strncmp(s1, s2, len)
char *s1, *s2;
int len;
{
    if (!s1 || !s2) return (-1);
    while (*s1 && *s2 && len > 0) {
	if (*s1 != *s2 && (tolower(*s1) != *s2)) return(-1);
	++s1; ++s2; --len;
    }
    if (len <= 0) return(0);
    return((*s1 == *s2) ? 0 : -1);
}

lc2strcmp(s1, s2)
char *s1, *s2;
{
    if (!s1 || !s2) return (-1);
    while (*s1 && *s2) {
	if (*s1 != *s2 && (tolower(*s1) != *s2)) return(-1);
	++s1; ++s2;
    }
    return((*s1 == *s2) ? 0 : -1);
}

char *getmyname() {
    static int initialized = 0;
#ifdef SYSV
    static struct utsname u;
    static char *hostname = u.nodename;
#else
    static char hostname[60];
#endif
    if (!initialized) {
#ifdef AMIGA
        strcpy(hostname, myAddress);
#else
#ifdef SYSV
    if (uname(&u) == -1) {
        hostname = "UNKNOWN.SITE.NAME";
    }
#else
#ifdef MSDOS
    strcpy(hostname, "UNKNOWN.SITE.NAME");
#else
        gethostname(hostname, sizeof(hostname));
#endif /* MSDOS */
#endif /* SYSV */
#endif /* AMIGA */
        initialized = 1;
    }
    return(hostname);
}

