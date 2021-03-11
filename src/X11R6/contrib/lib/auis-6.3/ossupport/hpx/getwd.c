#include <sys/param.h>

char *getwd(PathName)
char *PathName; {
    return((char*)getcwd(PathName, MAXPATHLEN));
}
